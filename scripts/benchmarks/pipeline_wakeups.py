#!/usr/bin/env python3
"""Compare production polling/activity wait paths with identical real-time HLS encoding."""
import argparse
import csv
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import statistics
import subprocess

ROOT = Path(__file__).resolve().parents[2]
POLLING_FILES = [
    'crates/engine/src/playout/pipeline/scheduler.rs',
    'crates/engine/src/playout/pipeline/demux.rs',
    'crates/engine/src/output/encoding/workers.rs',
    'crates/engine/src/output/delivery.rs',
]


def capture(*command, cwd=ROOT):
    return subprocess.check_output(command, cwd=cwd, text=True).strip()


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def snapshot(directory, polling_ref=None):
    directory.mkdir(parents=True)
    shutil.copytree(ROOT / 'crates/engine', directory / 'crates/engine')
    manifest = (ROOT / 'Cargo.toml').read_text().replace(
        'members = ["crates/app", "crates/engine"]', 'members = ["crates/engine"]')
    (directory / 'Cargo.toml').write_text(manifest)
    shutil.copy2(ROOT / 'Cargo.lock', directory / 'Cargo.lock')
    if polling_ref:
        for relative in POLLING_FILES:
            content = capture('git', 'show', f'{polling_ref}:{relative}') + '\n'
            if relative.endswith('scheduler.rs'):
                # Preserve the current deferred-logo contract in both builds.
                old = 'output.apply_logo_overlay(&mut item.frame, logo, item.opacity);'
                assert old in content, 'polling scheduler is incompatible'
                content = content.replace(old,
                    'output.apply_shared_logo_overlay(&mut item.frame, logo, item.opacity);', 1)
                content = content.replace(
                    'output.apply_shared_logo_overlay(&mut item.frame, logo, item.opacity);\n                        }',
                    'output.apply_shared_logo_overlay(&mut item.frame, logo, item.opacity);\n'
                    '                        } else {\n                            output.clear_logo_overlay();\n                        }', 1)
            (directory / relative).write_text(content)
        queue = directory / 'crates/engine/src/utils/pipeline_queue.rs'
        content = queue.read_text().replace(
            '#[cfg(test)]\n    pub(crate) fn recv_timeout', 'pub(crate) fn recv_timeout', 1)
        queue.write_text(content)
    return {str(path.relative_to(directory)): digest(path)
            for path in (directory / 'crates/engine').rglob('*.rs')}


def build(snapshot_path, destination, log):
    command = ['cargo', 'build', '--release', '-p', 'ff-engine', '--no-default-features',
               '--features', 'tokio', '--example', 'pipeline_wakeups', '--message-format=json']
    environment = {**os.environ, 'CARGO_TARGET_DIR': str(ROOT / 'target')}
    with log.open('w') as output:
        result = subprocess.run(command, cwd=snapshot_path, env=environment,
                                stdout=subprocess.PIPE, stderr=output, text=True, check=True)
    for line in result.stdout.splitlines():
        artifact = json.loads(line)
        if (artifact.get('reason') == 'compiler-artifact'
                and artifact.get('target', {}).get('name') == 'pipeline_wakeups'
                and artifact.get('executable')):
            shutil.copy2(artifact['executable'], destination)
            return
    raise RuntimeError('Cargo did not report benchmark executable')


def validate_hls(directory, expected_duration):
    playlist = (directory / 'index.m3u8').read_text()
    duration = sum(float(line.split(':', 1)[1].rstrip(','))
                   for line in playlist.splitlines() if line.startswith('#EXTINF:'))
    assert abs(duration - expected_duration) < 0.3, (duration, expected_duration)
    packets = 0
    for segment in [line for line in playlist.splitlines() if line and not line.startswith('#')]:
        path = directory / segment
        assert path.is_file() and path.stat().st_size > 0
        probe = json.loads(capture('ffprobe', '-v', 'error', '-select_streams', 'v:0',
            '-count_packets', '-show_entries', 'stream=nb_read_packets,width,height',
            '-of', 'json', str(path)))
        stream = probe['streams'][0]
        assert (stream['width'], stream['height']) == (1280, 720)
        packets += int(stream['nb_read_packets'])
    assert abs(packets - round(expected_duration * 25)) <= 2, packets
    return {'hls_duration_s': duration, 'hls_video_packets': packets}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input', type=Path,
                        default=ROOT / 'tests_assets/storage/media_filler/filler_0.mp4')
    parser.add_argument('--repetitions', type=int, default=3)
    parser.add_argument('--warmup-clips', type=int, default=1)
    parser.add_argument('--measured-clips', type=int, default=2)
    parser.add_argument('--polling-ref', default='0452dbcb')
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'tmp/bench/pipeline-wakeups')
    args = parser.parse_args()
    if min(args.repetitions, args.warmup_clips, args.measured_clips) < 1:
        parser.error('counts must be positive')
    args.input = args.input.resolve(strict=True)
    report = args.output_dir.resolve() / datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S.%fZ')
    report.mkdir(parents=True)
    shutil.copy2(Path(__file__), report / 'pipeline_wakeups.py')
    source_duration = float(capture('ffprobe', '-v', 'error', '-show_entries', 'format=duration',
                                    '-of', 'default=noprint_wrappers=1:nokey=1', str(args.input)))
    metadata = {'revision': capture('git', 'rev-parse', 'HEAD'),
                'status': capture('git', 'status', '--short'),
                'polling_ref': capture('git', 'rev-parse', args.polling_ref),
                'input_sha256': digest(args.input), 'input_duration_s': source_duration,
                'platform': platform.platform(), 'rustc': capture('rustc', '-Vv'),
                'arguments': vars(args), 'source_hashes': {}, 'binary_sha256': {},
                'build_environment': {key: value for key, value in os.environ.items()
                    if key == 'RUSTFLAGS' or key.startswith(('CARGO_PROFILE_', 'CARGO_ENCODED_'))},
                'methodology': 'Same current composition, encoding and queue bounds in both builds; '
                'polling restores scheduler (including queue inspections), demux, encoding-worker and delivery paths. Current queue '
                'activity API remains present but unsubscribed in polling build. Fresh processes, '
                'warmup excluded, alternating A/B then B/A. No concurrent builds during measurement. '
                'CPU/context switches from process getrusage; startup/finalization excluded; '
                'peak RSS includes warmup. HLS duration, packet counts and resolution validated.'}
    for variant in ['polling', 'activity']:
        print(f'Building {variant}…', flush=True)
        source = report / 'sources' / variant
        metadata['source_hashes'][variant] = snapshot(source,
            args.polling_ref if variant == 'polling' else None)
        binary = report / variant
        build(source, binary, report / f'build-{variant}.log')
        metadata['binary_sha256'][variant] = digest(binary)
    (report / 'metadata.json').write_text(json.dumps(metadata, indent=2, default=str) + '\n')
    rows = []
    expected = source_duration * (args.warmup_clips + args.measured_clips)
    for repetition in range(args.repetitions):
        order = ['polling', 'activity'] if repetition % 2 == 0 else ['activity', 'polling']
        for variant in order:
            label = f'{repetition + 1}-{variant}'
            print(f'Run {label}…', flush=True)
            output = report / label
            command = [str(report / variant), '--input', str(args.input), '--output', str(output),
                       '--warmup-clips', str(args.warmup_clips),
                       '--measured-clips', str(args.measured_clips)]
            with (report / f'{label}.log').open('w') as error:
                result = subprocess.run(command, stdout=subprocess.PIPE, stderr=error, text=True,
                                        check=True, timeout=expected + 60)
            row = json.loads(result.stdout)
            row.update(variant=variant, repetition=repetition + 1)
            assert abs(row['wall_s'] - source_duration * args.measured_clips) < 1.0, row
            row.update(validate_hls(output, expected))
            rows.append(row)
            with (report / 'runs.jsonl').open('a') as file:
                file.write(json.dumps(row) + '\n')
            print(f"CPU={row['cpu_percent']:.2f}% wall={row['wall_s']:.3f}s packets={row['hls_video_packets']}", flush=True)
    with (report / 'runs.csv').open('w') as file:
        writer = csv.DictWriter(file, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    summary = {variant: {key: statistics.median(row[key] for row in rows if row['variant'] == variant)
                        for key in ['cpu_s', 'cpu_percent', 'wall_s', 'voluntary_switches',
                                    'involuntary_switches', 'peak_rss_bytes', 'finish_s']}
               for variant in ['polling', 'activity']}
    summary['paired_cpu_change_percent'] = [
        (next(row['cpu_s'] for row in rows if row['repetition'] == repetition and row['variant'] == 'activity')
         / next(row['cpu_s'] for row in rows if row['repetition'] == repetition and row['variant'] == 'polling') - 1) * 100
        for repetition in range(1, args.repetitions + 1)]
    summary['aggregate'] = {
        variant: {
            'cpu_s': sum(row['cpu_s'] for row in rows if row['variant'] == variant),
            'cpu_percent': 100 * sum(row['cpu_s'] for row in rows if row['variant'] == variant)
                / sum(row['wall_s'] for row in rows if row['variant'] == variant),
        } for variant in ['polling', 'activity']
    }
    (report / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))
    print(f'Reports: {report}')


if __name__ == '__main__':
    main()

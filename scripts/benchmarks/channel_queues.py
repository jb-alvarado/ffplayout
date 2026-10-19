#!/usr/bin/env python3
"""Build and compare channel adapters in fresh processes; no playout/DB access."""

import argparse
import csv
from datetime import datetime, timezone
import json
import hashlib
import os
from pathlib import Path
import platform
import random
import statistics
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[2]
BACKENDS = ['pipeline', 'std', 'crossbeam', 'kanal', 'flume', 'rtrb', 'ringbuf']
SCENARIOS = ['throughput', 'mpsc', 'realtime', 'burst', 'slow', 'idle']


def selections(value, available):
    items = value.split(',')
    if not items or any(item not in available for item in items):
        raise argparse.ArgumentTypeError(f'Choose comma-separated values from {available}')
    return list(dict.fromkeys(items))


def capacities(value):
    try:
        items = list(dict.fromkeys(int(item) for item in value.split(',')))
    except ValueError as error:
        raise argparse.ArgumentTypeError('Capacities must be integers') from error
    if not items or any(not 1 <= item <= 4096 for item in items):
        raise argparse.ArgumentTypeError('Capacities must be between 1 and 4096')
    return items


def capture(*command):
    result = subprocess.run(command, cwd=ROOT, text=True, capture_output=True, check=True)
    return result.stdout.strip()


def binary_digest(path):
    digest = hashlib.sha256()
    with path.open('rb') as source:
        for block in iter(lambda: source.read(1024 * 1024), b''):
            digest.update(block)
    return digest.hexdigest()


def cpu_description():
    if sys.platform == 'darwin':
        try:
            return capture('sysctl', '-n', 'machdep.cpu.brand_string')
        except (OSError, subprocess.SubprocessError):
            return platform.processor() or 'unavailable'
    cpuinfo = Path('/proc/cpuinfo')
    if cpuinfo.exists():
        for line in cpuinfo.read_text().splitlines():
            if line.startswith(('model name', 'Hardware')):
                return line.split(':', 1)[-1].strip()
    return platform.processor()


def build():
    # JSON compiler artifacts avoid guessing Cargo's executable hash/path.
    command = ['cargo', 'bench', '-p', 'ff-engine', '--no-default-features', '--features', 'tokio',
               '--bench', 'channel_queues', '--no-run', '--message-format=json']
    print('Building release benchmark…', flush=True)
    result = subprocess.run(command, cwd=ROOT, text=True, stdout=subprocess.PIPE, check=True)
    for line in reversed(result.stdout.splitlines()):
        artifact = json.loads(line)
        if (artifact.get('reason') == 'compiler-artifact'
                and artifact.get('target', {}).get('name') == 'channel_queues'
                and artifact.get('executable')):
            return Path(artifact['executable'])
    raise RuntimeError('Cargo did not report the benchmark executable')


def write_csv(path, rows):
    if not rows:
        return
    with path.open('w', newline='') as output:
        writer = csv.DictWriter(output, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def summarize(rows):
    groups = {}
    for row in rows:
        key = row['scenario'], row['capacity'], row['backend']
        groups.setdefault(key, []).append(row)
    summary = []
    for (scenario, capacity, backend), values in sorted(groups.items()):
        result = {'scenario': scenario, 'capacity': capacity, 'backend': backend, 'runs': len(values)}
        for field in ['messages_s', 'cpu_percent', 'cpu_s', 'handoff_p50_us',
                      'handoff_p99_us', 'send_p99_us', 'schedule_late_p99_us',
                      'voluntary_switches', 'involuntary_switches']:
            result[f'{field}_median'] = statistics.median(row[field] for row in values)
        result['cpu_ns_per_message_median'] = statistics.median(
            row['cpu_s'] * 1_000_000_000 / row['messages'] for row in values)
        result['handoff_max_us_worst'] = max(row['handoff_max_us'] for row in values)
        result['peak_rss_bytes_median'] = statistics.median(row['peak_rss_bytes'] for row in values)
        result['peak_rss_bytes_worst'] = max(row['peak_rss_bytes'] for row in values)
        summary.append(result)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--backends', type=lambda value: selections(value, BACKENDS), default=BACKENDS)
    parser.add_argument('--scenarios', type=lambda value: selections(value, SCENARIOS), default=SCENARIOS)
    parser.add_argument('--capacities', type=capacities, default=[24])
    parser.add_argument('--seconds', type=float, help='Duration of paced scenarios (default 5)')
    parser.add_argument('--messages', type=int, help='Messages per producer in unpaced scenarios (default 100000)')
    parser.add_argument('--repetitions', type=int, help='Independent processes per case (default 3)')
    parser.add_argument('--quick', action='store_true', help='Smoke test: .25 s, 2000 messages, one repetition')
    parser.add_argument('--seed', type=int, default=7813)
    parser.add_argument('--binary', type=Path, help='Use an already built benchmark executable')
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'target' / 'channel-benchmarks')
    args = parser.parse_args()
    args.seconds = args.seconds if args.seconds is not None else (0.25 if args.quick else 5.0)
    args.messages = args.messages if args.messages is not None else (2000 if args.quick else 100_000)
    args.repetitions = args.repetitions if args.repetitions is not None else (1 if args.quick else 3)
    if not 0.1 <= args.seconds <= 3600 or not 1 <= args.messages <= 10_000_000 or not 1 <= args.repetitions <= 100:
        parser.error('Require seconds 0.1–3600, messages 1–10000000 and repetitions 1–100')
    binary = args.binary.resolve() if args.binary else build()
    report = args.output_dir.resolve() / datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S.%fZ')
    report.mkdir(parents=True, exist_ok=False)
    metadata = {
        'schema': 1, 'revision': capture('git', 'rev-parse', 'HEAD'),
        'git_status': capture('git', 'status', '--short'),
        'rustc': capture('rustc', '-Vv'), 'platform': platform.platform(),
        'machine': platform.machine(), 'logical_cpus': os.cpu_count(),
        'cpu': cpu_description(),
        'cpu_affinity': sorted(os.sched_getaffinity(0)) if hasattr(os, 'sched_getaffinity') else None,
        'build_environment': {key: value for key, value in os.environ.items()
                              if key in ['RUSTFLAGS', 'CARGO_ENCODED_RUSTFLAGS']
                              or key.startswith(('CARGO_PROFILE_RELEASE_', 'CARGO_PROFILE_BENCH_'))},
        'binary': str(binary), 'arguments': vars(args),
        'binary_sha256': binary_digest(binary),
        'methodology': 'Fresh process per case; warmup 2048 messages per producer; count bounds only; '
                       'pooled Arc payloads; ring buffers include park/unpark adapters; '
                       'RSS includes measurement storage; p99 summaries are medians of run p99s.',
    }
    (report / 'metadata.json').write_text(json.dumps(metadata, indent=2, default=str) + '\n')
    (report / 'Cargo.lock').write_bytes((ROOT / 'Cargo.lock').read_bytes())
    for source in [ROOT / 'crates/engine/benches/channel_queues.rs',
                   ROOT / 'crates/engine/src/utils/pipeline_queue.rs', Path(__file__)]:
        (report / source.name).write_bytes(source.read_bytes())
    rows, failures = [], []
    randomizer = random.Random(args.seed)
    cases = [(scenario, capacity, backend) for scenario in args.scenarios for capacity in args.capacities
             for backend in args.backends if not (scenario == 'mpsc' and backend in ['rtrb', 'ringbuf'])]
    for repetition in range(args.repetitions):
        order = cases.copy()
        randomizer.shuffle(order)
        for scenario, capacity, backend in order:
            print(f'Run {repetition + 1}/{args.repetitions}: {scenario}, capacity={capacity}, {backend}', flush=True)
            command = [str(binary), '--backend', backend, '--scenario', scenario,
                       '--capacity', str(capacity), '--seconds', str(args.seconds), '--messages', str(args.messages)]
            timeout = max(60, args.seconds + args.messages * 0.002 / 128 + 30)
            try:
                result = subprocess.run(command, text=True, capture_output=True, timeout=timeout, check=True)
                row = json.loads(result.stdout)
                row['repetition'] = repetition + 1
                rows.append(row)
                with (report / 'runs.jsonl').open('a') as output:
                    output.write(json.dumps(row) + '\n')
            except (subprocess.SubprocessError, ValueError) as error:
                failures.append({'command': command, 'error': str(error),
                                 'stderr': str(getattr(error, 'stderr', ''))})
                print(f'FAILED: {error}', file=sys.stderr)
    summary = summarize(rows)
    write_csv(report / 'runs.csv', rows)
    write_csv(report / 'summary.csv', summary)
    (report / 'failures.json').write_text(json.dumps(failures, indent=2) + '\n')
    print('\nScenario    Cap Backend     CPU% med  handoff p99 µs med  peak RSS MiB med')
    for row in summary:
        print(f"{row['scenario']:11} {row['capacity']:3} {row['backend']:10} "
              f"{row['cpu_percent_median']:8.2f} {row['handoff_p99_us_median']:19.2f} "
              f"{row['peak_rss_bytes_median'] / 1048576:17.2f}")
    print(f'\nReports: {report}')
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())

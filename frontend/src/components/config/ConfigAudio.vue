<script setup lang="ts">
import { useI18n } from 'vue-i18n'

import { useConfig } from '@/stores/config'
import { useIndex } from '@/stores/index'

const { t } = useI18n()
const configStore = useConfig()
const indexStore = useIndex()

async function saveAudio() {
    const response = await configStore.setPlayoutConfig(configStore.playout)
    if (!response.ok) {
        indexStore.msgAlert('error', await response.text(), 3)
        return
    }

    indexStore.msgAlert('success', t('config.updatePlayoutSuccess'), 2)
    await configStore.getPlayoutConfig()
}
</script>

<template>
    <div class="max-w-300 xs:pe-8">
        <h2 class="pt-3 text-3xl">Audio</h2>
        <form v-if="configStore.playout" class="mt-10 max-w-3xl" @submit.prevent="saveAudio">
            <fieldset class="fieldset">
                <legend class="fieldset-legend">Volume</legend>
                <input v-model.number="configStore.playout.audio.volume" type="number" min="0" max="1.5" step="0.001" class="input input-sm w-36" />
            </fieldset>
            <fieldset class="fieldset mt-5 rounded-box w-full">
                <label class="fieldset-label text-base-content">
                    <input v-model="configStore.playout.audio.live_loudness_enable" type="checkbox" class="checkbox" />
                    Live ingest loudness normalization (EBU R128)
                </label>
                <p class="fieldset-label items-baseline">Changes take effect immediately; only live ingest is processed.</p>
            </fieldset>
            <div v-if="configStore.playout.audio.live_loudness_enable" class="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                <label class="fieldset"><span class="fieldset-legend">Target LUFS</span><input v-model.number="configStore.playout.audio.live_loudness_target_lufs" type="number" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">Dead band (LU)</span><input v-model.number="configStore.playout.audio.live_loudness_dead_band_lu" type="number" min="0" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">True peak ceiling (dBTP)</span><input v-model.number="configStore.playout.audio.live_loudness_true_peak_ceiling_dbtp" type="number" max="0" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">Maximum gain (dB)</span><input v-model.number="configStore.playout.audio.live_loudness_max_gain_db" type="number" min="0" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">Maximum attenuation (dB)</span><input v-model.number="configStore.playout.audio.live_loudness_max_attenuation_db" type="number" max="0" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">Silence gate (LUFS)</span><input v-model.number="configStore.playout.audio.live_loudness_silence_gate_lufs" type="number" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">Gain up (dB/s)</span><input v-model.number="configStore.playout.audio.live_loudness_gain_up_db_per_second" type="number" min="0" step="0.1" class="input input-sm w-full" /></label>
                <label class="fieldset"><span class="fieldset-legend">Gain down (dB/s)</span><input v-model.number="configStore.playout.audio.live_loudness_gain_down_db_per_second" type="number" min="0" step="0.1" class="input input-sm w-full" /></label>
            </div>
            <button class="btn btn-primary mt-6" type="submit">{{ t('config.save') }}</button>
        </form>
    </div>
</template>

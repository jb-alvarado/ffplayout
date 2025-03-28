<template>
    <div class="w-full min-h-screen xs:h-full flex justify-center items-center">
        <div v-if="authStore.isLogin" class="flex flex-col justify-center items-center w-full p-5">
            <SystemStats v-if="configStore.channels.length > 0" />

            <div class="w-full flex flex-wrap justify-center gap-1 md:gap-0 md:join mt-5">
                <NuxtLink :to="localePath({ name: 'player' })" class="btn btn-primary join-item px-2">
                    {{ t('button.player') }}
                </NuxtLink>
                <NuxtLink :to="localePath({ name: 'media' })" class="btn btn-primary join-item px-2">
                    {{ t('button.media') }}
                </NuxtLink>
                <NuxtLink
                    v-if="configStore.playout?.text?.add_text && !configStore.playout?.text?.text_from_filename"
                    :to="localePath({ name: 'message' })"
                    class="btn btn-primary join-item px-2"
                >
                    {{ t('button.message') }}
                </NuxtLink>
                <NuxtLink :to="localePath({ name: 'logging' })" class="btn btn-primary join-item px-2">
                    {{ t('button.logging') }}
                </NuxtLink>
                <NuxtLink :to="localePath({ name: 'configure' })" class="btn btn-primary join-item px-2">
                    {{ t('button.configure') }}
                </NuxtLink>
                <button class="btn btn-primary join-item px-2" @click="logout()">
                    {{ t('button.logout') }}
                </button>
                <div class="dropdown">
                    <div tabindex="0" role="button" class="btn btn-primary bg-primary/70 px-2 join-item">{{ selectedLang?.name }}</div>
                    <ul tabindex="0" class="dropdown-content menu bg-base-100 rounded-box z-1 w-52 p-2 shadow-sm">
                        <li v-for="lang in locales" :key="lang.short" :title="lang.long">
                            <button class="px-1 py-2 rounded" @click="changeLang(lang)">
                                {{ lang.name }}
                            </button>
                        </li>
                    </ul>
                </div>
                <label class="join-item btn btn-primary swap swap-rotate">
                    <input type="checkbox" :checked="indexStore.darkMode" @change="toggleDarkTheme" />
                    <SvgIcon name="swap-on" classes="w-5 h-5" />
                    <SvgIcon name="swap-off" classes="w-5 h-5" />
                </label>
            </div>
        </div>
        <div v-else class="w-96 min-w-full flex flex-col justify-center items-center px-4">
            <h1 class="text-6xl xs:text-8xl">ffplayout</h1>

            <form class="mt-10" @submit.prevent="login">
                <input
                    v-model="formUsername"
                    type="text"
                    name="username"
                    :placeholder="t('input.username')"
                    class="input w-full focus:border-base-content/30 focus:outline-base-content/30"
                    required
                />

                <input
                    v-model="formPassword"
                    type="password"
                    name="password"
                    :placeholder="t('input.password')"
                    class="input w-full mt-5 focus:border-base-content/30 focus:outline-base-content/30"
                    required
                />

                <div class="w-full mt-4 grid grid-flow-row-dense grid-cols-12 grid-rows-1 gap-2">
                    <div class="col-span-3">
                        <button type="submit" class="btn btn-primary">
                            {{ t('button.login') }}
                        </button>
                    </div>
                    <div class="col-span-12 sm:col-span-9">
                        <div
                            v-if="showLoginError"
                            role="alert"
                            class="alert alert-error w-auto rounded-sm z-2 h-12 p-[0.7rem]"
                        >
                            <SvgIcon name="error" />
                            <span>{{ formError }}</span>
                        </div>
                    </div>
                </div>
            </form>
        </div>
    </div>
</template>

<script setup lang="ts">
const colorMode = useColorMode()
const { locale, locales, t } = useI18n()
const localePath = useLocalePath()
const switchLocalePath = useSwitchLocalePath()
const router = useRouter()

const authStore = useAuth()
const configStore = useConfig()
const indexStore = useIndex()

const selectedLang = ref()
const formError = ref('')
const showLoginError = ref(false)
const formUsername = ref('')
const formPassword = ref('')

const langCookie = useCookie('i18n_redirected')

onMounted(() => {
    selectedLang.value = locales.value.find((loc: any) => loc.code === locale.value)
})

async function login() {
    try {
        const status = await authStore.obtainToken(formUsername.value, formPassword.value)

        formUsername.value = ''
        formPassword.value = ''
        formError.value = ''

        if (status === 401 || status === 400 || status === 403) {
            formError.value = t('alert.wrongLogin')
            showLoginError.value = true

            setTimeout(() => {
                showLoginError.value = false
            }, 3000)
        }

        await configStore.configInit()
    } catch (e) {
        formError.value = e as string
    }
}

function toggleDarkTheme() {
    indexStore.darkMode = !indexStore.darkMode

    if (indexStore.darkMode) {
        colorMode.preference = 'dark'
    } else {
        colorMode.preference = 'light'
    }
}

async function logout() {
    try {
        authStore.removeToken()
    } catch (e) {
        formError.value = e as string
    }
}

async function changeLang(lang: any) {
    const path = switchLocalePath(lang.code)

    selectedLang.value = lang
    langCookie.value = lang.code

    router.push(path)
}
</script>

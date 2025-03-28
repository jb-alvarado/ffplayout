import tailwindcss from '@tailwindcss/vite'

// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
    devServer: {
        port: 3000, // default: 3000
        host: '127.0.0.1', // default: localhost
    },

    devtools: { enabled: false },

    nitro: {
        devProxy: {
            '/api': { target: 'http://127.0.0.1:8787/api' },
            '/auth': { target: 'http://127.0.0.1:8787/auth' },
            '/data': { target: 'http://127.0.0.1:8787/data' },
            '/live': { target: 'http://127.0.0.1:8787/live' },
            '/1/live': { target: 'http://127.0.0.1:8787/1/live' },
            '/file': { target: 'http://127.0.0.1:8787/file' },
            '/hls': { target: 'http://127.0.0.1:8787/hls' },
        },
    },

    ignore: ['**/public/tv-media**', '**/public/Videos**', '**/public/live**', '**/public/home**'],
    ssr: false,

    // debug: false,

    app: {
        head: {
            title: 'ffplayout',
            meta: [
                {
                    charset: 'utf-8',
                },
                {
                    name: 'viewport',
                    content: 'width=device-width, initial-scale=1',
                },
                {
                    name: 'description',
                    content: 'Frontend for ffplayout, the 24/7 streaming solution.',
                },
            ],
            link: [
                {
                    rel: 'icon',
                    type: 'image/x-icon',
                    href: '/favicon.ico',
                },
            ],
        },
    },

    modules: ['@nuxt/eslint', '@nuxtjs/color-mode', '@nuxtjs/i18n', '@pinia/nuxt', '@vueuse/nuxt'],

    css: ['@/assets/css/main.css'],

    colorMode: {
        preference: 'dark', // default value of $colorMode.preference
        fallback: 'system', // fallback value if not system preference found
        hid: 'nuxt-color-mode-script',
        globalName: '__NUXT_COLOR_MODE__',
        componentName: 'ColorScheme',
        classPrefix: '',
        classSuffix: '',
        dataValue: 'theme',
        storageKey: 'theme',
    },

    i18n: {
        locales: [
            {
                code: 'de',
                language: 'de-DE',
                name: 'Deutsch',
                file: 'de-DE.js',
            },
            {
                code: 'en',
                language: 'en-US',
                name: 'English',
                file: 'en-US.js',
            },
            {
                code: 'pt-br',
                language: 'pt-BR',
                name: 'Português (BR)',
                file: 'pt-BR.js',
            },
            {
                code: 'ru',
                language: 'ru-RU',
                name: 'Русский язык (RU)',
                file: 'ru-RU.js',
            },
        ],
        customRoutes: 'config',
        pages: {
            player: {
                de: '/wiedergabe',
                en: '/player',
                'pt-br': '/player',
                ru: '/player',
            },
            media: {
                de: '/medien',
                en: '/media',
                'pt-br': '/armazenamento',
                ru: '/media',
            },
            message: {
                de: '/nachrichten',
                en: '/message',
                'pt-br': '/legenda',
                ru: '/message',
            },
            logging: {
                de: '/protokollierung',
                en: '/logging',
                'pt-br': '/registro',
                ru: '/logging',
            },
            configure: {
                de: '/einstellungen',
                en: '/configure',
                'pt-br': '/configurar',
                ru: '/configure',
            },
        },
        detectBrowserLanguage: {
            useCookie: true,
            alwaysRedirect: true,
        },
        langDir: 'locales',
        defaultLocale: 'en',

        compilation: {
            strictMessage: false,
        },
    },

    vite: {
        build: {
            chunkSizeWarningLimit: 800000,
        },
        plugins: [tailwindcss()],
    },

    experimental: {
        payloadExtraction: false,
    },

    compatibilityDate: '2025-03-20',
})

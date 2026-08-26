import DefaultPlusTheme from '@lando/vitepress-theme-default-plus'
import DefaultTheme from 'vitepress/theme'
import type { Theme } from 'vitepress'
import { h, onMounted, watch, nextTick } from 'vue'
import { useRoute, useData } from 'vitepress'
import mediumZoom from 'medium-zoom'
import ApiPage from './components/ApiPage.vue'
import ApiFilterControls from './components/ApiFilterControls.vue'
import ApiMembers from './components/ApiMembers.vue'
import { applySidebarFilter } from './sidebarFilter'
import { showInherited, showProtected } from './apiFilterState'
import './custom.css'

export default {
  extends: DefaultPlusTheme,
  Layout() {
    const { frontmatter } = useData()
    const isApi = frontmatter.value?.docType === 'api' || !!frontmatter.value?.unit

    return h(DefaultTheme.Layout, null, {
      'doc-before': () => (isApi ? h(ApiPage) : null),
      'aside-top': () => h(ApiFilterControls)
    })
  },
  enhanceApp({ app }) {
    app.component('ApiPage', ApiPage)
    app.component('ApiFilterControls', ApiFilterControls)
    app.component('ApiMembers', ApiMembers)
  },
  setup() {
    const route = useRoute()
    const { frontmatter } = useData()

    const initZoom = () => {
      mediumZoom('.vp-doc img, .content img, main img', { background: 'var(--vp-c-bg)' })
    }

    const updateApiPageClass = () => {
      if (typeof document !== 'undefined') {
        const isApi = frontmatter.value?.docType === 'api' || !!frontmatter.value?.unit
        document.body.classList.toggle('api-page-doc', isApi)
      }
    }

    onMounted(() => {
      initZoom()
      updateApiPageClass()
      nextTick(() => setTimeout(applySidebarFilter, 50))
    })

    watch(
      [() => route.path, showInherited, showProtected],
      () => {
        updateApiPageClass()
        nextTick(() => setTimeout(applySidebarFilter, 50))
      }
    )
  }
} satisfies Theme

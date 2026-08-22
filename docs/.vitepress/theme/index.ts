import DefaultPlusTheme from '@lando/vitepress-theme-default-plus'
import DefaultTheme from 'vitepress/theme'
import type { Theme } from 'vitepress'
import { h, onMounted, watch, nextTick } from 'vue'
import { useRoute, useData } from 'vitepress'
import mediumZoom from 'medium-zoom'
import ApiPage from './components/ApiPage.vue'
import './custom.css'

export default {
  extends: DefaultPlusTheme,
  Layout() {
    const { frontmatter } = useData()
    if (frontmatter.value?.docType === 'api' || frontmatter.value?.unit) {
      return h(DefaultTheme.Layout, null, {
        'doc-before': () => h(ApiPage)
      })
    }
    return h(DefaultTheme.Layout)
  },
  enhanceApp({ app }) {
    app.component('ApiPage', ApiPage)
  },
  setup() {
    const route = useRoute()
    const initZoom = () => {
      mediumZoom('.vp-doc img', { background: 'var(--vp-c-bg)' })
    }
    onMounted(() => {
      initZoom()
    })
    watch(
      () => route.path,
      () => nextTick(() => initZoom())
    )
  }
} satisfies Theme

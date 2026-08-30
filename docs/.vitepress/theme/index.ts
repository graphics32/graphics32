import DefaultPlusTheme from '@lando/vitepress-theme-default-plus'
import DefaultTheme from 'vitepress/theme'
import type { Theme } from 'vitepress'
import { h, onMounted, watch, watchEffect, nextTick } from 'vue'
import { useRoute, useData } from 'vitepress'
import mediumZoom from 'medium-zoom'
import ApiPage from './components/ApiPage.vue'
import ApiFilterControls from './components/ApiFilterControls.vue'
import ApiMembers from './components/ApiMembers.vue'
import HeroCarousel from './components/HeroCarousel.vue'
import BuildFooter from './components/BuildFooter.vue'
import { applySidebarFilter } from './sidebarFilter'
import { showInherited, showProtected } from './apiFilterState'
import './custom.css'

export default {
  extends: DefaultPlusTheme,
  Layout() {
    const { frontmatter } = useData()
    const isApi = frontmatter.value?.docType === 'api' || !!frontmatter.value?.unit

    const isHome = frontmatter.value?.layout === 'home'

    return h(DefaultTheme.Layout, null, {
      'doc-before': () => (isApi ? h(ApiPage) : null),
      'aside-top': () => h(ApiFilterControls),
      'home-hero-image': () => h(HeroCarousel),
      'layout-bottom': () => (isHome ? h(BuildFooter) : null)
    })
  },
  enhanceApp({ app }) {
    app.component('ApiPage', ApiPage)
    app.component('ApiFilterControls', ApiFilterControls)
    app.component('ApiMembers', ApiMembers)
    app.component('HeroCarousel', HeroCarousel)
    app.component('BuildFooter', BuildFooter)
  },
  setup() {
    const route = useRoute()
    const { frontmatter, theme, page } = useData()

    watchEffect(() => {
      if (theme.value?.editLink) {
        if (frontmatter.value?.isVirtual === 'true' || frontmatter.value?.isVirtual === true) {
          const filePath = page.value?.filePath || ''
          const lastSlash = filePath.lastIndexOf('/')
          const dirPath = lastSlash >= 0 ? filePath.slice(0, lastSlash) : ''
          const fileName = lastSlash >= 0 ? filePath.slice(lastSlash + 1) : filePath
          const valueStr = frontmatter.value?.templateValue || ''

          const basePattern = typeof theme.value.editLink.pattern === 'string' ? theme.value.editLink.pattern : ''
          const match = basePattern.match(/^https:\/\/github\.com\/[^\/]+\/[^\/]+\/(?:edit|new)\/([^\/]+)\//)
          const branch = match ? match[1] : 'documentation'

          theme.value.editLink.pattern = `https://github.com/graphics32/graphics32/new/${branch}/docs/${dirPath}?filename=${encodeURIComponent(fileName)}&value=${encodeURIComponent(valueStr)}`
          theme.value.editLink.text = 'Create this page on GitHub'
        } else {
          const basePattern = typeof theme.value.editLink.pattern === 'string' ? theme.value.editLink.pattern : ''
          const match = basePattern.match(/^https:\/\/github\.com\/[^\/]+\/[^\/]+\/(?:edit|new)\/([^\/]+)\//)
          const branch = match ? match[1] : 'documentation'

          theme.value.editLink.pattern = `https://github.com/graphics32/graphics32/edit/${branch}/docs/:path`
          theme.value.editLink.text = 'Edit this page on GitHub'
        }
      }
    })

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

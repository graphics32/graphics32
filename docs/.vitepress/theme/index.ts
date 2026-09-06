import DefaultPlusTheme from '@lando/vitepress-theme-default-plus'
import DefaultTheme from 'vitepress/theme'
import type { Theme } from 'vitepress'
import { h, onMounted, watch, watchEffect, nextTick } from 'vue'
import { useRoute, useData } from 'vitepress'
import 'photoswipe/style.css'
import {
  NolebaseEnhancedReadabilitiesMenu,
  NolebaseEnhancedReadabilitiesScreenMenu,
} from '@nolebase/vitepress-plugin-enhanced-readabilities/client'
import '@nolebase/vitepress-plugin-enhanced-readabilities/client/style.css'

import ApiPage from './components/ApiPage.vue'
import ApiFilterControls from './components/ApiFilterControls.vue'
import ApiMembers from './components/ApiMembers.vue'
import HeroCarousel from './components/HeroCarousel.vue'
import BuildFooter from './components/BuildFooter.vue'
import { applySidebarFilter } from './sidebarFilter'
import { showInherited, showProtected, showAbstract } from './apiFilterState'
import './custom.css'

let siteDataRefInstance: any = null
let sidebarLoaded = false

const loadApiSidebar = () => {
  if (sidebarLoaded || typeof window === 'undefined') return
  sidebarLoaded = true

  const baseUrl = (import.meta.env.BASE_URL || '/').replace(/\/$/, '') + '/'
  const sidebarUrl = `${baseUrl}sidebarData.json`

  fetch(sidebarUrl)
    .then((res) => {
      if (!res.ok) throw new Error(`Failed to fetch ${sidebarUrl}`)
      return res.json()
    })
    .then((data) => {
      if (siteDataRefInstance && siteDataRefInstance.value) {
        siteDataRefInstance.value = {
          ...siteDataRefInstance.value,
          themeConfig: {
            ...siteDataRefInstance.value.themeConfig,
            sidebar: {
              ...siteDataRefInstance.value.themeConfig?.sidebar,
              '/api/': data
            }
          }
        }
        nextTick(() => setTimeout(applySidebarFilter, 50))
      }
    })
    .catch((err) => {
      console.warn('[theme] Error loading sidebarData.json:', err)
    })
}

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
      'layout-bottom': () => (isHome ? h(BuildFooter) : null),
      'nav-bar-content-after': () => h(NolebaseEnhancedReadabilitiesMenu),
      'nav-screen-content-after': () => h(NolebaseEnhancedReadabilitiesScreenMenu)
    })
  },
  enhanceApp({ app, siteData }) {
    siteDataRefInstance = siteData
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

          const fm = frontmatter.value || {}
          const lines = ['---']
          lines.push('layout: doc')
          lines.push('docType: api')
          if (fm.unit) lines.push(`unit: ${fm.unit}`)
          if (fm.parent) lines.push(`parent: ${fm.parent}`)
          if (fm.entity) lines.push(`entity: ${fm.entity}`)
          if (fm.kind) lines.push(`kind: ${fm.kind}`)
          if (fm.scope) lines.push(`scope: ${fm.scope}`)
          if (fm.summary) {
            lines.push(`summary: "${fm.summary}"`)
          } else {
            lines.push('summary: "<required>"')
          }

          if (fm.overloads && Array.isArray(fm.overloads)) {
            lines.push('overloads:')
            for (const ov of fm.overloads) {
              lines.push(`  - declaration: "${ov.declaration || ''}"`)
              lines.push('    parameters:')
              if (ov.parameters && Array.isArray(ov.parameters)) {
                for (const p of ov.parameters) {
                  lines.push(`      - name: ${p.name || '<required>'}`)
                  lines.push(`        type: ${p.type || '<required>'}`)
                  lines.push(`        description: "${p.description || '<required>'}"`)
                }
              }
            }
          } else {
            if (fm.declaration) {
              lines.push(`declaration: "${fm.declaration}"`)
            } else {
              lines.push('declaration: "<required>"')
            }

            if (fm.parameters && Array.isArray(fm.parameters)) {
              lines.push('parameters:')
              for (const p of fm.parameters) {
                lines.push(`  - name: ${p.name || '<required>'}`)
                lines.push(`    type: ${p.type || '<required>'}`)
                lines.push(`    description: "${p.description || '<required>'}"`)
              }
            } else {
              lines.push('parameters:')
              lines.push('  - name: <required>')
              lines.push('    type: <required>')
              lines.push('    description: "<required>"')
            }
          }

          lines.push('---')
          lines.push('')
          lines.push('## Remarks')
          lines.push('')
          lines.push('<required>')

          const valueStr = lines.join('\n')

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

    let pswpListenerAttached = false

    const initPhotoSwipe = () => {
      if (typeof window === 'undefined' || pswpListenerAttached) return
      pswpListenerAttached = true

      document.body.addEventListener('click', (e: MouseEvent) => {
        const target = e.target as HTMLElement | null
        if (!target || target.tagName !== 'IMG') return

        if (!target.matches('.vp-doc img, .content img, main img:not(.hero-slide img)')) return

        const img = target as HTMLImageElement
        if (!img.src) return

        e.preventDefault()

        const allImages = Array.from(
          document.querySelectorAll<HTMLImageElement>('.vp-doc img, .content img, main img:not(.hero-slide img)')
        ).filter((i) => i.src)

        const index = allImages.indexOf(img)
        const dataSource = (allImages.length > 0 ? allImages : [img]).map((i) => ({
          src: i.currentSrc || i.src,
          msrc: i.currentSrc || i.src,
          w: i.naturalWidth || i.clientWidth || 800,
          h: i.naturalHeight || i.clientHeight || 600,
          element: i,
          alt: i.alt || ''
        }))

        import('photoswipe').then(({ default: PhotoSwipe }) => {
          const pswp = new PhotoSwipe({
            dataSource,
            index: index >= 0 ? index : 0,
            showHideAnimationType: 'zoom',
            bgOpacity: 0.8,
            close: true,
            zoom: false,
            counter: false,
            arrowKeys: false,
            arrowPrev: false,
            arrowNext: false,
          })
          pswp.init()
        })
      })
    }

    const updateApiPageClass = () => {
      if (typeof document !== 'undefined') {
        const isApi = frontmatter.value?.docType === 'api' || !!frontmatter.value?.unit
        document.body.classList.toggle('api-page-doc', isApi)
      }
    }

    onMounted(() => {
      nextTick(() => initPhotoSwipe())
      updateApiPageClass()
      loadApiSidebar()
      nextTick(() => setTimeout(applySidebarFilter, 50))
    })

    watch(
      [() => route.path, showInherited, showProtected, showAbstract],
      () => {
        updateApiPageClass()
        nextTick(() => {
          setTimeout(applySidebarFilter, 50)
        })
      }
    )
  }
} satisfies Theme

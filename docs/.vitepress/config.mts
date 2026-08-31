import { defineConfig } from '@lando/vitepress-theme-default-plus/config'
import { withMermaid } from 'vitepress-plugin-mermaid'
import fs from 'fs'
import path from 'path'
import mathjax3 from 'markdown-it-mathjax3'
import { generateSidebarForDir } from './sidebar'
import { buildSymbolMap, apiSymbolLinksPlugin } from './symbolMap'
import { apiShortcodesPlugin } from './shortcodePlugin'
import { branchShortcodePlugin } from './branchShortcodePlugin'
import { execSync } from 'child_process'
import { generateVirtualMembers, getGitBranch } from './virtualMembers'
import { generateMemberData } from './generateMemberData'

const stackOverflowSvg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor">
  <path d="M18.986 21.865v-6.404h2.134V24H1.844v-8.539h2.13v6.404h15.012zM6.111 19.731h10.657v-2.134H6.111v2.134zm.284-5.263l10.42 2.217.447-2.086-10.42-2.217-.447 2.086zm1.393-5.021l9.467 4.908.974-1.89-9.467-4.908-.974 1.89zm3.018-4.703l7.808 7.272 1.442-1.551-7.808-7.272-1.442 1.551zm5.228-3.911l-1.821 1.09 5.485 9.155 1.821-1.09-5.485-9.155z"/>
</svg>`

// Load Delphi-PRAXIS English & German favicon SVGs
let delphiPraxisEnSvg = ''
let delphiPraxisDeSvg = ''
try {
  delphiPraxisEnSvg = fs.readFileSync(path.resolve(__dirname, '../public/delphipraxis-en.svg'), 'utf-8')
  delphiPraxisDeSvg = fs.readFileSync(path.resolve(__dirname, '../public/delphipraxis-de.svg'), 'utf-8')
} catch (e) {
  delphiPraxisEnSvg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor"><circle cx="12" cy="12" r="10"/></svg>`
  delphiPraxisDeSvg = delphiPraxisEnSvg
}

const apiDir = path.resolve(__dirname, '../api')
const examplesDir = path.resolve(__dirname, '../examples')

const currentBranch = getGitBranch()

let shortCommit = ''
try {
  shortCommit = execSync('git rev-parse --short HEAD', { encoding: 'utf-8' }).trim()
} catch (e) {
  shortCommit = 'unknown'
}

const buildTimestamp = new Date().toISOString().replace('T', ' ').slice(0, 19) + ' UTC'

// Generate virtual member routes for class inheritance before building symbol map and sidebar
generateVirtualMembers(apiDir)
generateMemberData(apiDir, path.resolve(__dirname, 'theme/memberData.json'))

const symbolMap = buildSymbolMap(apiDir)

/*
** Hard coded sidebars
*/
const guideSidebar = [
  {
    text: 'Guide',
    items: [
      { text: 'Getting Started', link: '/guide/' },
      { text: 'Features', link: '/guide/features' },
      { text: 'Requirements', link: '/guide/requirements' },
      { text: 'Installation', link: '/guide/installation' }
    ]
  }
]

const conceptsSidebar = [
  {
    text: 'Concepts',
    items: [
      { text: 'Drawing and Blending', link: '/guide/drawing-and-blending' },
      { text: 'Resampling and Transforms', link: '/guide/resampling-and-transforms' },
      { text: 'CPU Feature Detection', link: '/guide/cpu-feature-detection' },
      { text: 'SIMD Optimizations', link: '/guide/simd-optimizations' },
      { text: 'Alpha Composition', link: '/guide/alpha-composition' },
      { text: 'Line Patterns', link: '/guide/line-patterns' },
      { text: 'Color Gradients', link: '/guide/color-gradients' },
      { text: 'Sampling and Rasterization', link: '/guide/sampling-and-rasterization' },
      { text: 'Back-Ends', link: '/guide/back-ends' },
      { text: 'Repaint Optimization', link: '/guide/repaint-optimization' },
      { text: 'Vectorial Polygon Rasterizer', link: '/guide/vpr' },
      { text: 'Naming Conventions', link: '/guide/naming-conventions' },
      {
        text: 'Using TImage32',
        items: [
          { text: 'Overview', link: '/guide/using-timage32/overview' },
          { text: 'Bitmap Image', link: '/guide/using-timage32/bitmap-image' },
          { text: 'Using Layers', link: '/guide/using-timage32/using-layers' },
          { text: 'Paint Stages', link: '/guide/using-timage32/paint-stages' }
        ]
      }
    ]
  }
]

const aboutSidebar = [
  {
    text: 'About',
    items: [
      { text: 'Contact', link: '/guide/contact' },
      { text: 'License', link: '/guide/license' },
      { text: 'Generator Guide', link: '/how_to_generate_documentation' },
      { text: 'API Generator Guide', link: '/how_to_generate_API_documentation' }
    ]
  }
]

export default withMermaid(defineConfig({
  title: "Graphics32",
  description: "A high-performance 32-bit graphics library for Delphi and Lazarus/FPC",
  cleanUrls: true,
  rewrites: {
    // Strips category subfolders from public URLs while preserving physical category organization on disk
    // e.g. api/GR32/Classes/TBitmap32/Methods/Draw.md -> api/GR32/TBitmap32/Draw
    // e.g. api/GR32_Filters/Routines/Invert.md -> api/GR32_Filters/Invert
    'api/:unit/:cat(Classes|Types|Routines|Constants|Variables|Interfaces)/:class/:memcat(Constructors|Methods|Properties|Events)/:member': 'api/:unit/:class/:member',
    'api/:unit/:cat(Classes|Types|Routines|Constants|Variables|Interfaces)/:class/:memcat(Constructors|Methods|Properties|Events)/:member.md': 'api/:unit/:class/:member.md',
    'api/:unit/:cat(Classes|Types|Routines|Constants|Variables|Interfaces)/:class/index.md': 'api/:unit/:class/index.md',
    'api/:unit/:cat(Classes|Types|Routines|Constants|Variables|Interfaces)/:class': 'api/:unit/:class',
    'api/:unit/:cat(Classes|Types|Routines|Constants|Variables|Interfaces)/:item': 'api/:unit/:item',
    'api/:unit/:cat(Classes|Types|Routines|Constants|Variables|Interfaces)/:item.md': 'api/:unit/:item.md'
  },
  lastUpdated: true,
  ignoreDeadLinks: true, // Keep this until we get our act together and fix all the dead links

  head: [
    ['link', { rel: 'icon', type: 'image/png', href: '/favicon.png' }]
  ],

  vite: {
    ssr: {
      noExternal: ['@lando/vitepress-theme-default-plus']
    }
  },

  markdown: {
    math: true,
    languageAlias: {
      pas: 'pascal',
      delphi: 'pascal'
    },
    config: (md) => {
      md.use(mathjax3)
      apiSymbolLinksPlugin(md, symbolMap)
      apiShortcodesPlugin(md)
      branchShortcodePlugin(md)
    }
  },

  themeConfig: {
    buildInfo: {
      branch: currentBranch,
      commit: shortCommit,
      timestamp: buildTimestamp
    },

    editLink: {
      pattern: `https://github.com/graphics32/graphics32/edit/${currentBranch}/docs/:path`,
      text: 'Edit this page on GitHub'
    },

    nav: [
      { text: 'Home', link: '/' },
      {
        text: 'Guide',
        items: [
          { text: 'Getting Started', link: '/guide/' },
          { text: 'Features', link: '/guide/features' },
          { text: 'Requirements', link: '/guide/requirements' },
          { text: 'Installation', link: '/guide/installation' }
        ]
      },
      {
        text: 'Concepts',
        items: [
          { text: 'CPU Feature Detection', link: '/guide/cpu-feature-detection' },
          { text: 'SIMD Optimizations', link: '/guide/simd-optimizations' },
          { text: 'Alpha composition', link: '/guide/alpha-composition' },
          { text: 'Line Patterns', link: '/guide/line-patterns' },
          { text: 'Color Gradients', link: '/guide/color-gradients' },
          { text: 'Sampling and Rasterization', link: '/guide/sampling-and-rasterization' },
          { text: 'Back-Ends', link: '/guide/back-ends' },
          { text: 'Repaint Optimization', link: '/guide/repaint-optimization' },
          { text: 'Naming Conventions', link: '/guide/naming-conventions' },
          { text: 'Vectorial Polygon Rasterizer', link: '/guide/vpr' },
          {
            text: 'Using TImage32',
            items: [
              { text: 'Overview', link: '/guide/using-timage32/overview' },
              { text: 'Bitmap Image', link: '/guide/using-timage32/bitmap-image' },
              { text: 'Using Layers', link: '/guide/using-timage32/using-layers' },
              { text: 'Paint Stages', link: '/guide/using-timage32/paint-stages' }
            ]
          }
        ]
      },
      { text: 'API Reference', link: '/api/' },
      { text: 'Examples', link: '/examples/' },
      {
        text: 'About',
        items: [
          { text: 'Contact', link: '/guide/contact' },
          { text: 'License', link: '/guide/license' },
          {
            text: 'About the documentation',
            items: [
              { text: 'Generator Guide', link: '/how_to_generate_documentation' },
              { text: 'API Generator Guide', link: '/how_to_generate_API_documentation' }
            ]
          }
        ]
      }
    ],

    sidebar: {
/* Dynamic Guide sidebar; Disabled
      '/guide/': [
        {
          text: 'Guide',
          items: [
            { text: 'Overview', link: '/guide/' },
            ...generateSidebarForDir(guideDir, '', { collapsed: false })
          ]
        }
      ],
*/
/*
** Hard coded Guide sidebar
*/

      '/guide/features': guideSidebar,
      '/guide/requirements': guideSidebar,
      '/guide/installation': guideSidebar,
      '/guide/bitmaps-and-colors': conceptsSidebar,
      '/guide/drawing-and-blending': conceptsSidebar,
      '/guide/resampling-and-transforms': conceptsSidebar,
      '/guide/cpu-feature-detection': conceptsSidebar,
      '/guide/simd-optimizations': conceptsSidebar,
      '/guide/alpha-composition': conceptsSidebar,
      '/guide/line-patterns': conceptsSidebar,
      '/guide/color-gradients': conceptsSidebar,
      '/guide/sampling-and-rasterization': conceptsSidebar,
      '/guide/back-ends': conceptsSidebar,
      '/guide/repaint-optimization': conceptsSidebar,
      '/guide/naming-conventions': conceptsSidebar,
      '/guide/vpr': conceptsSidebar,
      '/guide/using-timage32': conceptsSidebar,
      '/guide/bitmap-image': conceptsSidebar,
      '/guide/using-layers': conceptsSidebar,
      '/guide/paint-stages': conceptsSidebar,
      '/guide/contact': aboutSidebar,
      '/guide/license': aboutSidebar,
      '/how_to_generate_documentation': aboutSidebar,
      '/how_to_generate_API_documentation': aboutSidebar,
      '/guide/': guideSidebar,
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'API Overview', link: '/api/' },
            ...generateSidebarForDir(apiDir, '', { collapsed: true })
          ]
        }
      ],
      '/examples/': [
        {
          text: 'Examples & Tutorials',
          items: [
            { text: 'Overview', link: '/examples/' },
            ...generateSidebarForDir(examplesDir, '', { collapsed: false })
          ]
        }
      ]
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/graphics32/graphics32' },
      {
        icon: { svg: stackOverflowSvg },
        link: 'https://stackoverflow.com/questions/tagged/graphics32',
        ariaLabel: 'Stack Overflow'
      },
      {
        icon: { svg: delphiPraxisEnSvg },
        link: 'https://en.delphipraxis.net/search/?q=graphics32',
        ariaLabel: 'Delphi-PRAXIS (English)'
      },
      {
        icon: { svg: delphiPraxisDeSvg },
        link: 'https://www.delphipraxis.net/dp_search.php?do=process&query=graphics32',
        ariaLabel: 'Delphi-PRAXIS (German)'
      }
    ],

    search: {
      provider: 'local'
    }
  }
}))

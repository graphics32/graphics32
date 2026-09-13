import type MarkdownIt from 'markdown-it'
import fs from 'fs'
import path from 'path'
import { fileURLToPath } from 'url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

interface ExampleMeta {
  src: string
  alt: string
  name: string
  path: string
  folder: string
  concept?: string
  techniques?: string
  api?: string[]
}

function loadExamplesData(): ExampleMeta[] {
  try {
    const jsonPath = path.resolve(__dirname, 'theme/exampleScreenshots.json')
    if (fs.existsSync(jsonPath)) {
      return JSON.parse(fs.readFileSync(jsonPath, 'utf-8'))
    }
  } catch (e) {
    console.error('Failed to load exampleScreenshots.json:', e)
  }
  return []
}

function generateExamplesTableMarkdown(examples: ExampleMeta[]): string {
  let md = '| Example | Demonstrates | Screenshot |\n'
  md += '| --- | --- | --- |\n'

  for (const ex of examples) {
    const encodedFolder = ex.folder.split('/').map(encodeURIComponent).join('/')
    const repoUrl = `https://github.com/graphics32/graphics32/tree/[branch]/Examples/${encodedFolder}`
    const link = `[${ex.name}](${repoUrl})`

    const conceptStr = ex.concept ? `**Concept:** ${ex.concept}` : ''
    const techStr = ex.techniques ? `**Techniques:** ${ex.techniques}` : ''
    const apiItems = ex.api && ex.api.length > 0 ? ex.api.map(a => `[[${a.replace(/\|/g, '\\|')}]]`).join(', ') : ''
    const apiStr = apiItems ? `**API:** ${apiItems}` : ''

    const demonstratesParts = [conceptStr, techStr, apiStr].filter(Boolean)
    const demonstrates = demonstratesParts.join('<br />')

    const screenshot = `<img src="${ex.src}" alt="${ex.name}" width="200" />`

    md += `| ${link} | ${demonstrates} | ${screenshot} |\n`
  }

  return md
}

export function apiShortcodesPlugin(md: MarkdownIt) {
  // Pre-process [examples] shortcode in block parsing phase so generated Markdown table tokens
  // are parsed into standard Markdown AST and handled by downstream plugins (symbol map, branch shortcode)
  md.core.ruler.before('normalize', 'examples-shortcode', (state) => {
    if (!state.src.includes('[examples]')) return

    const examples = loadExamplesData()
    if (examples.length === 0) return

    const tableMd = generateExamplesTableMarkdown(examples)
    state.src = state.src.replace(/\[examples\](?!\()/gi, tableMd)
  })

  md.core.ruler.after('inline', 'api-shortcodes', (state) => {
    for (const blockToken of state.tokens) {
      if (blockToken.type !== 'inline' || !blockToken.children) continue

      const newChildren = []
      for (const token of blockToken.children) {
        if (token.type !== 'text') {
          newChildren.push(token)
          continue
        }

        const text = token.content
        const regex = /\[(constructors|methods|properties|events|members|classes|types|routines|constants|interfaces)\](?!\()/gi
        let lastIndex = 0
        let match: RegExpExecArray | null

        while ((match = regex.exec(text)) !== null) {
          const matchIndex = match.index
          const codeName = match[1].toLowerCase()

          if (matchIndex > lastIndex) {
            const textToken = new state.Token('text', '', 0)
            textToken.content = text.slice(lastIndex, matchIndex)
            newChildren.push(textToken)
          }

          const htmlToken = new state.Token('html_inline', '', 0)
          htmlToken.content = `<ApiMembers type="${codeName}" />`
          newChildren.push(htmlToken)

          lastIndex = regex.lastIndex
        }

        if (lastIndex < text.length) {
          const textToken = new state.Token('text', '', 0)
          textToken.content = text.slice(lastIndex)
          newChildren.push(textToken)
        }
      }

      blockToken.children = newChildren
    }
  })
}

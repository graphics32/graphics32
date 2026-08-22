import fs from 'fs'
import path from 'path'
import type MarkdownIt from 'markdown-it'

export interface SymbolMap {
  [symbol: string]: string
}

/**
  Builds a mapping of API symbols to their relative doc links by scanning docs/api
 */
export function buildSymbolMap(apiRootDir: string): SymbolMap {
  const map: SymbolMap = {}

  function scan(dir: string) {
    if (!fs.existsSync(dir)) return
    const entries = fs.readdirSync(dir, { withFileTypes: true })
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name)
      if (entry.isDirectory()) {
        scan(fullPath)
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        try {
          const content = fs.readFileSync(fullPath, 'utf-8')
          const entityMatch = content.match(/^entity:\s*["']?(.*?)["']?$/m)
          const unitMatch = content.match(/^unit:\s*["']?(.*?)["']?$/m)

          let relLink = '/' + path.relative(path.resolve(apiRootDir, '..'), fullPath).replace(/\\/g, '/').replace(/\.md$/, '')
          if (relLink.endsWith('/index')) {
            relLink = relLink.slice(0, -5)
          }

          if (entityMatch && entityMatch[1]) {
            const entity = entityMatch[1].trim()
            map[entity] = relLink
            if (unitMatch && unitMatch[1]) {
              const unit = unitMatch[1].trim()
              map[`${unit}.${entity}`] = relLink
            }
          }
        } catch (e) {
          // ignore
        }
      }
    }
  }

  scan(apiRootDir)
  return map
}

/**
  Markdown-it plugin to resolve [[SymbolName]] or [[SymbolName|Label]] short links
 */
export function apiSymbolLinksPlugin(md: MarkdownIt, symbolMap: SymbolMap) {
  md.core.ruler.after('inline', 'api-symbol-links', (state) => {
    for (const blockToken of state.tokens) {
      if (blockToken.type !== 'inline' || !blockToken.children) continue

      const newChildren = []
      for (const token of blockToken.children) {
        if (token.type !== 'text') {
          newChildren.push(token)
          continue
        }

        const text = token.content
        const regex = /\[\[\s*([^\]|]+?)(?:\s*\|\s*([^\]]+))?\s*\]\]/g
        let lastIndex = 0
        let match: RegExpExecArray | null

        while ((match = regex.exec(text)) !== null) {
          const matchIndex = match.index
          const fullMatch = match[0]
          const rawSymbol = match[1].trim()
          const label = match[2] ? match[2].trim() : rawSymbol

          // Push text prior to match
          if (matchIndex > lastIndex) {
            const textToken = new state.Token('text', '', 0)
            textToken.content = text.slice(lastIndex, matchIndex)
            newChildren.push(textToken)
          }

          const targetUrl = symbolMap[rawSymbol]
          if (targetUrl) {
            const linkOpen = new state.Token('link_open', 'a', 1)
            linkOpen.attrs = [['href', targetUrl]]

            const linkText = new state.Token('text', '', 0)
            linkText.content = label

            const linkClose = new state.Token('link_close', 'a', -1)

            newChildren.push(linkOpen, linkText, linkClose)
          } else {
            // If symbol not found, render label or raw match as code/text
            const unmappedText = new state.Token('text', '', 0)
            unmappedText.content = label
            newChildren.push(unmappedText)
          }

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

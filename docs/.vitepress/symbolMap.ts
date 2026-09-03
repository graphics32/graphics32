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

          let relParts = path.relative(path.resolve(apiRootDir, '..'), fullPath).split(/[/\\]/)
          const fileNameNoExt = path.basename(entry.name, '.md')
          const parentDirName = path.basename(path.dirname(fullPath))
          const isMemberIndex = fileNameNoExt.toLowerCase() === 'index' && ['Constructors', 'Methods', 'Properties', 'Events'].includes(parentDirName)

          if (isMemberIndex) {
            relParts[relParts.length - 1] = `${parentDirName}-${fileNameNoExt}.md`
          }

          relParts = relParts.filter(part => !['Classes', 'Types', 'Routines', 'Constants', 'Variables', 'Interfaces', 'Constructors', 'Methods', 'Properties', 'Events'].includes(part))
          let relLink = '/' + relParts.join('/').replace(/\.md$/, '')
          if (relLink.endsWith('/index')) {
            relLink = relLink.slice(0, -5)
          }

          const aliases: string[] = []
          const aliasInlineMatch = content.match(/^aliases:\s*(.*)$/m)
          if (aliasInlineMatch) {
            const lineVal = aliasInlineMatch[1].trim()
            if (lineVal.startsWith('[')) {
              const inner = lineVal.slice(1, lineVal.endsWith(']') ? -1 : undefined)
              inner.split(',').forEach(s => {
                const item = s.trim().replace(/^["']|["']$/g, '')
                if (item) aliases.push(item)
              })
            } else if (lineVal.length > 0 && !lineVal.startsWith('#')) {
              lineVal.split(',').forEach(s => {
                const item = s.trim().replace(/^["']|["']$/g, '')
                if (item) aliases.push(item)
              })
            } else {
              const aliasBlockMatch = content.match(/aliases:\r?\n((?:\s*-\s*.*\r?\n?)+)/)
              if (aliasBlockMatch) {
                aliasBlockMatch[1].split(/\r?\n/).forEach(line => {
                  const item = line.replace(/^\s*-\s*/, '').trim().replace(/^["']|["']$/g, '')
                  if (item) aliases.push(item)
                })
              }
            }
          }

          const unit = unitMatch && unitMatch[1] ? unitMatch[1].trim() : undefined

          if (entityMatch && entityMatch[1]) {
            const entity = entityMatch[1].trim()
            map[entity] = relLink
            if (unit) {
              map[`${unit}.${entity}`] = relLink
            }
          }

          for (const alias of aliases) {
            if (!map[alias]) map[alias] = relLink
            if (unit && !map[`${unit}.${alias}`]) map[`${unit}.${alias}`] = relLink
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

export interface SymbolContext {
  parent?: string
  entity?: string
  unit?: string
  inheritance?: string[]
}

/**
  Helper function to resolve symbol in given page context using hierarchical scoping rules:
  1. If page has parent class 'Class' or entity 'Class' (e.g. TBitmap32), check 'Class.Symbol'
  2. Check ancestor classes from inheritance array
  3. If page has unit 'Unit' (e.g. GR32), check 'Unit.Symbol'
  4. Check global 'Symbol'
 */
export function resolveSymbol(
  rawSymbol: string,
  symbolMap: SymbolMap,
  context?: SymbolContext
): string | undefined {
  let targetClass = context?.parent
  if (!targetClass && context?.entity && !context.entity.includes('.')) {
    targetClass = context.entity
  }

  if (targetClass) {
    const classScoped = `${targetClass}.${rawSymbol}`
    if (symbolMap[classScoped]) {
      return symbolMap[classScoped]
    }

    if (context?.inheritance && Array.isArray(context.inheritance)) {
      const ancestors = [...context.inheritance].reverse()
      for (const ancestor of ancestors) {
        if (ancestor === targetClass) continue
        const ancestorScoped = `${ancestor}.${rawSymbol}`
        if (symbolMap[ancestorScoped]) {
          return symbolMap[ancestorScoped]
        }
      }
    }
  }

  if (context?.unit) {
    const unitScoped = `${context.unit}.${rawSymbol}`
    if (symbolMap[unitScoped]) {
      return symbolMap[unitScoped]
    }
  }

  if (symbolMap[rawSymbol]) {
    return symbolMap[rawSymbol]
  }

  return undefined
}

/**
  Markdown-it plugin to resolve [[SymbolName]] or [[SymbolName|Label]] short links
 */
export function apiSymbolLinksPlugin(md: MarkdownIt, symbolMap: SymbolMap) {
  md.core.ruler.after('inline', 'api-symbol-links', (state) => {
    // Extract frontmatter metadata from markdown-it environment
    const env = state.env || {}
    const frontmatter = env.frontmatter || {}
    const context: SymbolContext = {
      parent: frontmatter.parent,
      entity: frontmatter.entity,
      unit: frontmatter.unit,
      inheritance: frontmatter.inheritance
    }

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
          const rawSymbol = match[1].trim()
          const customLabel = match[2] ? match[2].trim() : undefined
          const label = customLabel !== undefined ? customLabel : rawSymbol

          // Push text prior to match
          if (matchIndex > lastIndex) {
            const textToken = new state.Token('text', '', 0)
            textToken.content = text.slice(lastIndex, matchIndex)
            newChildren.push(textToken)
          }

          const targetUrl = resolveSymbol(rawSymbol, symbolMap, context)
          if (targetUrl) {
            const linkOpen = new state.Token('link_open', 'a', 1)
            linkOpen.attrs = [['href', targetUrl]]

            const linkText = new state.Token('text', '', 0)
            linkText.content = label

            const linkClose = new state.Token('link_close', 'a', -1)

            newChildren.push(linkOpen, linkText, linkClose)
          } else {
            console.warn(`[symbolMap] Warning: Unresolved symbolic link '[[${rawSymbol}${customLabel ? '|' + customLabel : ''}]]' in ${env.relativePath || 'unknown page'}`)

            if (customLabel !== undefined) {
              // If custom label was explicitly provided, render as plain text
              const unmappedText = new state.Token('text', '', 0)
              unmappedText.content = customLabel
              newChildren.push(unmappedText)
            } else {
              // If only symbol was provided, render as code block
              const codeToken = new state.Token('code_inline', 'code', 0)
              codeToken.content = rawSymbol
              newChildren.push(codeToken)
            }
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

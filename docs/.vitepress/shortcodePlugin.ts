import type MarkdownIt from 'markdown-it'

export function apiShortcodesPlugin(md: MarkdownIt) {
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
        const regex = /\[(constructors|methods|properties|events|members)\](?!\()/gi
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

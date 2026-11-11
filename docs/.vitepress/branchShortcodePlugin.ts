import type MarkdownIt from 'markdown-it'
import { getGitBranch } from './virtualMembers'

export function branchShortcodePlugin(md: MarkdownIt) {
  const branch = getGitBranch()

  md.core.ruler.after('inline', 'branch-shortcode', (state) => {
    for (const blockToken of state.tokens) {
      if (blockToken.type !== 'inline' || !blockToken.children) continue

      for (const token of blockToken.children) {
        // Replace [branch] and percent-encoded %5Bbranch%5D in token attributes (e.g. href in link_open)
        if (token.attrs) {
          for (const attr of token.attrs) {
            if (attr[1]) {
              attr[1] = attr[1]
                .replace(/\[branch\]/gi, branch)
                .replace(/%5Bbranch%5D/gi, branch)
            }
          }
        }

        // Replace [branch] and percent-encoded %5Bbranch%5D in token content (e.g. text tokens)
        if (token.content) {
          token.content = token.content
            .replace(/\[branch\]/gi, branch)
            .replace(/%5Bbranch%5D/gi, branch)
        }
      }
    }
  })
}

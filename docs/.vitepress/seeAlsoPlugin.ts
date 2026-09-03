import type MarkdownIt from 'markdown-it'

/**
 * Markdown-it plugin to render a "## See also" section at the end of the markdown content
 * whenever frontmatter.seealso is present.
 */
export function seeAlsoPlugin(md: MarkdownIt) {
  md.core.ruler.before('inline', 'api-see-also', (state) => {
    const env = state.env || {}
    const frontmatter = env.frontmatter || {}

    if (!frontmatter.seealso) return
    if (env._seeAlsoAppended) return

    let items: string[] = []
    if (Array.isArray(frontmatter.seealso)) {
      items = frontmatter.seealso.map((i: any) => String(i).trim()).filter(Boolean)
    } else if (typeof frontmatter.seealso === 'string') {
      items = frontmatter.seealso
        .split(/\r?\n/)
        .map(s => s.trim().replace(/^-\s*/, ''))
        .filter(Boolean)
    }

    if (items.length === 0) return

    env._seeAlsoAppended = true

    const seeAlsoText = '\n\n## See also\n\n' + items.map(item => `- ${item}`).join('\n') + '\n'
    const newTokens: any[] = []
    md.block.parse(seeAlsoText, md, env, newTokens)
    state.tokens.push(...newTokens)
  })
}

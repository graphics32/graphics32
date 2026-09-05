import type MarkdownIt from 'markdown-it'

function normalizedReturns(returns: any): Array<{ type: string; description: string }> {
  if (!returns) return []
  if (Array.isArray(returns)) return returns
  if (typeof returns === 'object') return [returns]
  return []
}

function formatCodeValue(val: any): string {
  if (!val) return ''
  const str = String(val).trim()
  if (str.startsWith('`') || str.includes('[[')) {
    return str
  }
  return `\`${str}\``
}

/**
 * Markdown-it plugin that reads structured frontmatter fields (declaration, parameters,
 * returns, overloads) and injects Markdown markup into the document token stream.
 */
export function apiFrontmatterPlugin(md: MarkdownIt) {
  md.core.ruler.before('inline', 'api-frontmatter-injector', (state) => {
    const env = state.env || {}
    const frontmatter = env.frontmatter || {}

    // Only process API pages with 'docType: api'
    if (frontmatter.docType !== 'api') return
    if (env._apiFrontmatterInjected) return

    env._apiFrontmatterInjected = true

    let injectedMarkdown = ''

    // 1. OVERLOADED SIGNATURES
    if (frontmatter.overloads && frontmatter.overloads.length) {
      // Declarations block
      const declarations = frontmatter.overloads.map((ov: any) => ov.signature).join('\n')
      injectedMarkdown += `\n\n## Declarations\n\n\`\`\`pascal\n${declarations}\n\`\`\`\n`

      // Overload Details section
      injectedMarkdown += `\n\n## Overload Details\n\n`
      frontmatter.overloads.forEach((ov: any, idx: number) => {
        injectedMarkdown += `### Overload ${idx + 1}\n\n\`\`\`pascal\n${ov.signature}\n\`\`\`\n`
        if (ov.summary) {
          injectedMarkdown += `\n*${ov.summary}*\n`
        }

        if (ov.parameters && ov.parameters.length) {
          injectedMarkdown += `\n| Parameter | Type | Description |\n| --- | --- | --- |\n`
          for (const param of ov.parameters) {
            injectedMarkdown += `| ${formatCodeValue(param.name)} | ${formatCodeValue(param.type)} | ${param.description || ''} |\n`
          }
        }

        const rets = normalizedReturns(ov.returns)
        if (rets.length) {
          injectedMarkdown += `\n| Type | Description |\n| --- | --- |\n`
          for (const ret of rets) {
            injectedMarkdown += `| ${formatCodeValue(ret.type)} | ${ret.description || ''} |\n`
          }
        }
      })
    }
    // 2. SINGLE SIGNATURE
    else {
      if (frontmatter.declaration) {
        injectedMarkdown += `\n\n## Declaration\n\n\`\`\`pascal\n${frontmatter.declaration}\n\`\`\`\n`
      }

      if (frontmatter.parameters && frontmatter.parameters.length) {
        injectedMarkdown += `\n\n## Parameters\n\n| Parameter | Type | Description |\n| --- | --- | --- |\n`
        for (const param of frontmatter.parameters) {
          injectedMarkdown += `| ${formatCodeValue(param.name)} | ${formatCodeValue(param.type)} | ${param.description || ''} |\n`
        }
      }

      const rets = normalizedReturns(frontmatter.returns)
      if (rets.length) {
        injectedMarkdown += `\n\n## Returns\n\n| Type | Description |\n| --- | --- |\n`
        for (const ret of rets) {
          injectedMarkdown += `| ${formatCodeValue(ret.type)} | ${ret.description || ''} |\n`
        }
      }
    }

    if (injectedMarkdown) {
      const newTokens: any[] = []
      // Parse dynamically generated markdown string into native markdown-it tokens
      md.block.parse(injectedMarkdown, md, env, newTokens)
      // Prepend before user markdown body tokens
      state.tokens.unshift(...newTokens)
    }
  })
}

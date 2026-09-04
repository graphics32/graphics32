import fs from 'fs'
import path from 'path'

/**
  Parses frontmatter fields from a Markdown file
 */
function parseFrontmatter(filePath: string): Record<string, any> {
  const result: Record<string, any> = {}
  try {
    const content = fs.readFileSync(filePath, 'utf-8')
    const match = content.match(/^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/)
    if (match) {
      const yaml = match[1]
      const lines = yaml.split(/\r?\n/)
      for (const line of lines) {
        const colonIdx = line.indexOf(':')
        if (colonIdx > 0 && !line.trim().startsWith('-')) {
          const key = line.slice(0, colonIdx).trim()
          let val = line.slice(colonIdx + 1).trim()
          if ((val.startsWith('"') && val.endsWith('"')) || (val.startsWith("'") && val.endsWith("'"))) {
            val = val.slice(1, -1)
          }
          result[key] = val
        }
      }
      // Extract inheritance array
      const inhMatch = yaml.match(/inheritance:\r?\n((?:\s*-\s*.*\r?\n?)+)/)
      if (inhMatch) {
        result.inheritance = inhMatch[1]
          .split(/\r?\n/)
          .map(l => l.replace(/^\s*-\s*/, '').trim())
          .filter(Boolean)
      }
    }
  } catch (e) {
    // ignore
  }
  return result
}

function isVirtualFile(filePath: string): boolean {
  if (!fs.existsSync(filePath)) return false
  const fm = parseFrontmatter(filePath)
  return fm.isVirtual === 'true' || fm.isVirtual === true
}

export function getGitBranch(): string {
  let branch = process.env.DOCS_BRANCH
  if (!branch) {
    try {
      const { execSync } = require('child_process')
      branch = execSync('git rev-parse --abbrev-ref HEAD', { encoding: 'utf-8' }).trim()
    } catch (e) {
      // fallback
    }
  }
  if (!branch || branch === 'HEAD') branch = 'documentation'
  return branch
}

export function buildTemplateFrontmatterValue(ancestorContent: string, className: string, memberName: string): string {
  const fm = parseFrontmatterContent(ancestorContent)

  const lines = ['---']
  lines.push('layout: doc')
  lines.push('docType: api')

  if (fm.unit) lines.push(`unit: ${fm.unit}`)
  lines.push(`parent: ${className}`)
  lines.push(`entity: ${className}.${memberName}`)

  if (fm.kind) lines.push(`kind: ${fm.kind}`)
  if (fm.scope) lines.push(`scope: ${fm.scope}`)

  if (fm.summary) {
    lines.push(`summary: "${fm.summary}"`)
  } else {
    lines.push('summary: "<required>"')
  }

  if (fm.overloadsBlock) {
    lines.push(fm.overloadsBlock)
  } else {
    if (fm.declaration) {
      lines.push(`declaration: "${fm.declaration}"`)
    } else {
      lines.push('declaration: "<required>"')
    }

    if (fm.parametersBlock) {
      lines.push(fm.parametersBlock)
    } else {
      lines.push('parameters:')
      lines.push('  - name: <required>')
      lines.push('    type: <required>')
      lines.push('    description: "<required>"')
    }

    if (fm.returnsBlock) {
      lines.push(fm.returnsBlock)
    }
  }

  lines.push('---')
  lines.push('')
  lines.push('## Remarks')
  lines.push('')
  lines.push('<required>')

  return lines.join('\n')
}

function parseFrontmatterContent(content: string): Record<string, any> {
  const result: Record<string, any> = {}
  const match = content.match(/^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/)
  if (!match) return result
  const yaml = match[1]

  const lines = yaml.split(/\r?\n/)
  for (const line of lines) {
    const colonIdx = line.indexOf(':')
    if (colonIdx > 0 && !line.trim().startsWith('-')) {
      const key = line.slice(0, colonIdx).trim()
      let val = line.slice(colonIdx + 1).trim()
      if ((val.startsWith('"') && val.endsWith('"')) || (val.startsWith("'") && val.endsWith("'"))) {
        val = val.slice(1, -1)
      }
      result[key] = val
    }
  }

  const declMatch = yaml.match(/^declaration:\s*(.*)$/m)
  if (declMatch) {
    let decl = declMatch[1].trim()
    if ((decl.startsWith('"') && decl.endsWith('"')) || (decl.startsWith("'") && decl.endsWith("'"))) {
      decl = decl.slice(1, -1)
    }
    result.declaration = decl
  }

  const paramMatch = yaml.match(/parameters:\r?\n((?:\s*-\s*.*\r?\n?)+)/)
  if (paramMatch) {
    result.parametersBlock = paramMatch[0].trim()
  }

  const retMatch = yaml.match(/returns:\r?\n([\s\S]*?)(?=\n[a-zA-Z0-9_-]+:|$)/)
  if (retMatch) {
    result.returnsBlock = retMatch[0].trim()
  }

  const ovMatch = yaml.match(/overloads:\r?\n([\s\S]*?)(?=\n[a-zA-Z0-9_-]+:|$)/)
  if (ovMatch) {
    result.overloadsBlock = ovMatch[0].trim()
  }

  return result
}

/**
  Generates virtual member files for derived classes based on ancestor class member files.
 */
export function generateVirtualMembers(apiRootDir: string) {
  if (!fs.existsSync(apiRootDir)) return

  // 1. Build index of all classes and their physical members
  const classMap: Record<string, { unit: string; classDir: string; members: Record<string, string> }> = {}

  const units = fs.readdirSync(apiRootDir, { withFileTypes: true }).filter(e => e.isDirectory())

  for (const unit of units) {
    const unitDir = path.join(apiRootDir, unit.name)
      // Scan classes directly under unit or under unit/Classes/
      const candidateClassDirs: { className: string; classDir: string }[] = []

      const categoryFolders = ['Classes', 'Types', 'Interfaces']
      const directDirs = fs.readdirSync(unitDir, { withFileTypes: true }).filter(e => e.isDirectory())
      for (const dir of directDirs) {
        if (categoryFolders.includes(dir.name)) {
          const catSubDir = path.join(unitDir, dir.name)
          const subDirs = fs.readdirSync(catSubDir, { withFileTypes: true }).filter(e => e.isDirectory())
          for (const subItem of subDirs) {
            candidateClassDirs.push({ className: subItem.name, classDir: path.join(catSubDir, subItem.name) })
          }
        } else if (!['Routines', 'Constants', 'Variables'].includes(dir.name)) {
          candidateClassDirs.push({ className: dir.name, classDir: path.join(unitDir, dir.name) })
        }
      }

      for (const { className, classDir } of candidateClassDirs) {
      const indexMd = path.join(classDir, 'index.md')
      if (!fs.existsSync(indexMd)) continue

      const members: Record<string, string> = {} // relMemberPath -> fullPath
      const memberFolders = ['Constructors', 'Methods', 'Properties', 'Events']

      for (const folder of memberFolders) {
        const folderDir = path.join(classDir, folder)
        if (fs.existsSync(folderDir)) {
          const mFiles = fs.readdirSync(folderDir).filter(f => f.endsWith('.md'))
          for (const mFile of mFiles) {
            const fullPath = path.join(folderDir, mFile)
            // Skip existing virtual member files when indexing human-authored members
            if (!isVirtualFile(fullPath)) {
              members[`${folder}/${mFile}`] = fullPath
            }
          }
        }
      }

        classMap[className] = {
        unit: unit.name,
        classDir,
        members
      }
    }
  }

  // 2. Process inheritance for each class
  for (const [className, info] of Object.entries(classMap)) {
    const indexMd = path.join(info.classDir, 'index.md')
    const fm = parseFrontmatter(indexMd)
    const ancestors = fm.inheritance || []

    const generatedForClass = new Set<string>()

    for (const ancestorName of [...ancestors].reverse()) {
      if (ancestorName === className || !classMap[ancestorName]) continue

      const ancestorInfo = classMap[ancestorName]
      for (const [relMemberPath, ancestorMemberPath] of Object.entries(ancestorInfo.members)) {
        if (generatedForClass.has(relMemberPath)) continue

        const targetMemberPath = path.join(info.classDir, relMemberPath)

        // Create or regenerate virtual page if member does NOT physically exist on derived class (or is an existing virtual page)
        if (!fs.existsSync(targetMemberPath) || isVirtualFile(targetMemberPath)) {
          try {
            const ancestorContent = fs.readFileSync(ancestorMemberPath, 'utf-8')
            const memberName = path.basename(relMemberPath, '.md')

            const branch = getGitBranch()

            // Calculate relPath relative to docs/ folder (e.g. api/GR32/TBitmap32/Methods/Clear.md)
            const docsDir = path.resolve(apiRootDir, '..')
            const targetRelPath = path.relative(docsDir, targetMemberPath).replace(/\\/g, '/')
            const dirPath = path.dirname(targetRelPath).replace(/\\/g, '/')
            const fileName = path.basename(targetRelPath)

            const valueStr = buildTemplateFrontmatterValue(ancestorContent, className, memberName)

            let newContent = ancestorContent
            const match = ancestorContent.match(/^---\r?\n([\s\S]*?)\r?\n---(\r?\n[\s\S]*)?$/)
            if (match) {
              let headFm = match[1]
              const body = match[2] || ''

              // Clean existing inheritedFrom, isVirtual, parent, entity, templateValue from headFm to avoid duplicate key errors
              headFm = headFm
                .replace(/^inheritedFrom:\s*.*$/m, '')
                .replace(/^isVirtual:\s*.*$/m, '')
                .replace(/^parent:\s*.*$/m, '')
                .replace(/^entity:\s*.*$/m, '')
                .replace(/^templateValue:\s*.*$/m, '')
                .split(/\r?\n/)
                .filter(l => l.trim().length > 0)
                .join('\n')

              const headFmPart = headFm.length > 0 ? `${headFm}\n` : ''
              newContent = `---\ninheritedFrom: ${ancestorName}.${memberName}\nisVirtual: true\nparent: ${className}\nentity: ${className}.${memberName}\n${headFmPart}---${body}`
            }

            fs.mkdirSync(path.dirname(targetMemberPath), { recursive: true })
            fs.writeFileSync(targetMemberPath, newContent, 'utf-8')
            generatedForClass.add(relMemberPath)
          } catch (e) {
            // ignore
          }
        }
      }
    }
  }
}

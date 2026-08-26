import fs from 'fs'
import path from 'path'

/**
  Parses frontmatter fields from a Markdown file
 */
function parseFrontmatter(filePath: string): Record<string, any> {
  const result: Record<string, any> = {}
  try {
    const content = fs.readFileSync(filePath, 'utf-8')
    if (content.startsWith('---')) {
      const secondDash = content.indexOf('---', 3)
      if (secondDash > 0) {
        const yaml = content.slice(3, secondDash)
        const lines = yaml.split(/\r?\n/)
        for (const line of lines) {
          const colonIdx = line.indexOf(':')
          if (colonIdx > 0 && !line.trim().startsWith('-')) {
            const key = line.slice(0, colonIdx).trim()
            let val = line.slice(colonIdx + 1).trim()
            if (val.startsWith('"') && val.endsWith('"')) val = val.slice(1, -1)
            if (val.startsWith("'") && val.endsWith("'")) val = val.slice(1, -1)
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

            let newContent = ancestorContent
            if (ancestorContent.startsWith('---')) {
              const secondDash = ancestorContent.indexOf('---', 3)
              if (secondDash > 0) {
                let headFm = ancestorContent.slice(3, secondDash)
                const body = ancestorContent.slice(secondDash)

                // Clean existing inheritedFrom, isVirtual, parent, entity from headFm to avoid duplicate key errors
                headFm = headFm
                  .replace(/^inheritedFrom:\s*.*$/m, '')
                  .replace(/^isVirtual:\s*.*$/m, '')
                  .replace(/^parent:\s*.*$/m, '')
                  .replace(/^entity:\s*.*$/m, '')
                  .split(/\r?\n/)
                  .filter(l => l.trim().length > 0)
                  .join('\n')

                const headFmPart = headFm.length > 0 ? `\n${headFm}` : ''
                newContent = `---\ninheritedFrom: ${ancestorName}.${memberName}\nisVirtual: true\nparent: ${className}\nentity: ${className}.${memberName}${headFmPart}\n${body}`
              }
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

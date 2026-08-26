import fs from 'fs'
import path from 'path'

export interface MemberInfo {
  unit: string
  parent: string
  entity: string
  name: string
  kind: string
  category: string
  scope: string
  summary: string
  declaration?: string
  inheritedFrom?: string
  isVirtual: boolean
  isProtected: boolean
  propertyType?: string
  link: string
}

export interface ClassMembersData {
  Constructors: MemberInfo[]
  Methods: MemberInfo[]
  Properties: MemberInfo[]
  Events: MemberInfo[]
}

export interface MemberDataFile {
  byLink: Record<string, MemberInfo>
  byClass: Record<string, ClassMembersData>
}

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
      }
    }
  } catch (e) {
    // ignore
  }
  return result
}

function extractPropertyType(declaration: string): string {
  if (!declaration) return ''
  // e.g. property Canvas: TCanvas read GetCanvas;
  // e.g. property Pixel[X, Y: Integer]: TColor32 read GetPixel;
  const match = declaration.match(/:\s*([A-Za-z0-9_<>, ]+?)(?:\s+read|\s+write|\s*;|$)/)
  if (match && match[1]) {
    return match[1].trim()
  }
  return ''
}

export function generateMemberData(apiRootDir: string, outputFile: string) {
  if (!fs.existsSync(apiRootDir)) return

  const byLink: Record<string, MemberInfo> = {}
  const byClass: Record<string, ClassMembersData> = {}

  const memberCategories = ['Constructors', 'Methods', 'Properties', 'Events'] as const

  function scanClassDir(classDir: string, unitName: string, className: string) {
    if (!byClass[className]) {
      byClass[className] = {
        Constructors: [],
        Methods: [],
        Properties: [],
        Events: []
      }
    }

    for (const category of memberCategories) {
      const categoryDir = path.join(classDir, category)
      if (!fs.existsSync(categoryDir)) continue

      const files = fs.readdirSync(categoryDir).filter(f => f.endsWith('.md'))
      for (const file of files) {
        const fullPath = path.join(categoryDir, file)
        const name = path.basename(file, '.md')
        const fm = parseFrontmatter(fullPath)

        const scope = fm.scope || 'Public'
        const isVirtual = fm.isVirtual === 'true' || fm.isVirtual === true || !!fm.inheritedFrom
        const isProtected = scope.toLowerCase() === 'protected'
        const link = `/api/${unitName}/${className}/${name}`
        const propertyType = extractPropertyType(fm.declaration || '')

        const info: MemberInfo = {
          unit: unitName,
          parent: className,
          entity: fm.entity || `${className}.${name}`,
          name,
          kind: fm.kind || category.slice(0, -1),
          category,
          scope,
          summary: fm.summary || '',
          declaration: fm.declaration,
          inheritedFrom: fm.inheritedFrom,
          isVirtual,
          isProtected,
          propertyType,
          link
        }

        byLink[link] = info
        byClass[className][category].push(info)
      }
    }
  }

  const units = fs.readdirSync(apiRootDir, { withFileTypes: true }).filter(e => e.isDirectory())
  for (const unit of units) {
    const unitDir = path.join(apiRootDir, unit.name)
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
      if (fs.existsSync(path.join(classDir, 'index.md'))) {
        scanClassDir(classDir, unit.name, className)
      }
    }
  }

  const result: MemberDataFile = { byLink, byClass }
  fs.mkdirSync(path.dirname(outputFile), { recursive: true })
  fs.writeFileSync(outputFile, JSON.stringify(result, null, 2), 'utf-8')
}

import fs from 'fs'
import path from 'path'
import yaml from 'js-yaml'
import { generateVirtualMembers } from './virtualMembers'

export interface ApiParameter {
  name?: string
  type?: string
  description?: string
}

export interface ApiReturn {
  type?: string
  description?: string
}

export interface ApiOverload {
  signature?: string
  summary?: string
  parameters?: ApiParameter[]
  returns?: ApiReturn[]
}

export interface ApiMemberSchema {
  name: string
  entity: string
  unit: string
  parent?: string
  kind: string
  scope?: string
  summary: string
  declaration?: string
  parameters?: ApiParameter[]
  returns?: ApiReturn[]
  overloads?: ApiOverload[]
  seealso?: string[]
  isVirtual?: boolean
  inheritedFrom?: string
  hidden?: boolean
  link: string
}

export interface ApiClassSchema {
  name: string
  entity: string
  unit: string
  kind: string
  scope?: string
  declaration?: string
  inheritance?: string[]
  summary: string
  isAbstract?: boolean
  hidden?: boolean
  link: string
  constructors: ApiMemberSchema[]
  methods: ApiMemberSchema[]
  properties: ApiMemberSchema[]
  events: ApiMemberSchema[]
  fields: ApiMemberSchema[]
}

export interface ApiUnitSchema {
  name: string
  entity: string
  kind: string
  summary: string
  link: string
  classes: ApiClassSchema[]
  interfaces: ApiClassSchema[]
  types: ApiMemberSchema[]
  routines: ApiMemberSchema[]
  constants: ApiMemberSchema[]
  variables: ApiMemberSchema[]
}

export interface ApiRootSchema {
  $schema: string
  title: string
  description: string
  version: string
  units: ApiUnitSchema[]
}

function parseFrontmatter(filePath: string): Record<string, any> {
  try {
    const content = fs.readFileSync(filePath, 'utf-8')
    const match = content.match(/^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/)
    if (match) {
      const parsed = yaml.load(match[1])
      if (parsed && typeof parsed === 'object') {
        return parsed as Record<string, any>
      }
    }
  } catch (e) {
    // ignore
  }
  return {}
}

function cleanString(val: any): string {
  if (val === undefined || val === null) return ''
  return String(val).trim()
}

function cleanArray<T>(val: any): T[] {
  if (!Array.isArray(val)) return []
  return val
}

function buildMemberSchema(filePath: string, unitName: string, className: string | undefined, name: string, category: string): ApiMemberSchema {
  const fm = parseFrontmatter(filePath)

  const isVirtual = fm.isVirtual === true || fm.isVirtual === 'true' || !!fm.inheritedFrom
  const linkName = name.toLowerCase() === 'index' ? `${category}-${name}` : name
  const link = className ? `/api/${unitName}/${className}/${linkName}` : `/api/${unitName}/${name}`

  return {
    name,
    entity: cleanString(fm.entity) || (className ? `${className}.${name}` : name),
    unit: unitName,
    parent: className || undefined,
    kind: cleanString(fm.kind) || category.slice(0, -1),
    scope: cleanString(fm.scope) || (className ? 'Public' : undefined),
    summary: cleanString(fm.summary),
    declaration: cleanString(fm.declaration) || undefined,
    parameters: cleanArray<ApiParameter>(fm.parameters),
    returns: cleanArray<ApiReturn>(fm.returns),
    overloads: cleanArray<ApiOverload>(fm.overloads),
    seealso: cleanArray<string>(fm.seealso),
    isVirtual: isVirtual || undefined,
    inheritedFrom: cleanString(fm.inheritedFrom) || undefined,
    hidden: fm.hidden === true || fm.hidden === 'true' || undefined,
    link
  }
}

export function generateLlmsTxt(apiRootDir: string, outputDir: string) {
  if (!fs.existsSync(apiRootDir)) return

  generateVirtualMembers(apiRootDir)

  const memberCategories = ['Constructors', 'Methods', 'Properties', 'Events', 'Fields'] as const

  const unitDirs = fs.readdirSync(apiRootDir, { withFileTypes: true })
    .filter(e => e.isDirectory())
    .map(e => e.name)
    .sort()

  const units: ApiUnitSchema[] = []

  for (const unitName of unitDirs) {
    const unitDir = path.join(apiRootDir, unitName)
    const unitIndexMd = path.join(unitDir, 'index.md')
    const unitFm = fs.existsSync(unitIndexMd) ? parseFrontmatter(unitIndexMd) : {}

    const unitSchema: ApiUnitSchema = {
      name: unitName,
      entity: cleanString(unitFm.entity) || unitName,
      kind: cleanString(unitFm.kind) || 'Unit',
      summary: cleanString(unitFm.summary),
      link: `/api/${unitName}`,
      classes: [],
      interfaces: [],
      types: [],
      routines: [],
      constants: [],
      variables: []
    }

    // 1. Scan Unit-level Categories (Types, Routines, Constants, Variables)
    const categoryScanMap: { cat: string; field: 'types' | 'routines' | 'constants' | 'variables' }[] = [
      { cat: 'Types', field: 'types' },
      { cat: 'Routines', field: 'routines' },
      { cat: 'Constants', field: 'constants' },
      { cat: 'Variables', field: 'variables' }
    ]

    for (const { cat, field } of categoryScanMap) {
      const catDir = path.join(unitDir, cat)
      if (!fs.existsSync(catDir)) continue

      const files = fs.readdirSync(catDir, { withFileTypes: true })
        .filter(f => f.isFile() && f.name.endsWith('.md'))
        .sort((a, b) => a.name.localeCompare(b.name))

      for (const file of files) {
        const fullPath = path.join(catDir, file.name)
        const name = path.basename(file.name, '.md')
        const mem = buildMemberSchema(fullPath, unitName, undefined, name, cat)
        unitSchema[field].push(mem)
      }
    }

    // 2. Scan Classes & Interfaces
    const candidateClassDirs: { className: string; classDir: string; category: 'Classes' | 'Interfaces' | 'Types' }[] = []

    const containerFolders = ['Classes', 'Interfaces', 'Types'] as const
    const directDirs = fs.readdirSync(unitDir, { withFileTypes: true }).filter(e => e.isDirectory())

    for (const dir of directDirs) {
      if ((containerFolders as readonly string[]).includes(dir.name)) {
        const catName = dir.name as 'Classes' | 'Interfaces' | 'Types'
        const catSubDir = path.join(unitDir, catName)
        const subDirs = fs.readdirSync(catSubDir, { withFileTypes: true }).filter(e => e.isDirectory())
        for (const subItem of subDirs) {
          candidateClassDirs.push({ className: subItem.name, classDir: path.join(catSubDir, subItem.name), category: catName })
        }
      } else if (!['Routines', 'Constants', 'Variables'].includes(dir.name)) {
        candidateClassDirs.push({ className: dir.name, classDir: path.join(unitDir, dir.name), category: 'Classes' })
      }
    }

    candidateClassDirs.sort((a, b) => a.className.localeCompare(b.className))

    for (const { className, classDir, category } of candidateClassDirs) {
      const indexMd = path.join(classDir, 'index.md')
      if (!fs.existsSync(indexMd)) continue

      const classFm = parseFrontmatter(indexMd)
      const isAbstract = classFm.abstract === true || classFm.abstract === 'true'
      const isHidden = classFm.hidden === true || classFm.hidden === 'true'
      const kind = cleanString(classFm.kind) || (category === 'Interfaces' ? 'Interface' : 'Class')

      const classSchema: ApiClassSchema = {
        name: className,
        entity: cleanString(classFm.entity) || className,
        unit: unitName,
        kind,
        scope: cleanString(classFm.scope) || 'Public',
        declaration: cleanString(classFm.declaration) || undefined,
        inheritance: cleanArray<string>(classFm.inheritance),
        summary: cleanString(classFm.summary),
        isAbstract: isAbstract || undefined,
        hidden: isHidden || undefined,
        link: `/api/${unitName}/${className}`,
        constructors: [],
        methods: [],
        properties: [],
        events: [],
        fields: []
      }

      for (const cat of memberCategories) {
        const memberDir = path.join(classDir, cat)
        if (!fs.existsSync(memberDir)) continue

        const fieldKey = cat.toLowerCase() as 'constructors' | 'methods' | 'properties' | 'events' | 'fields'
        const mFiles = fs.readdirSync(memberDir, { withFileTypes: true })
          .filter(f => f.isFile() && f.name.endsWith('.md'))
          .sort((a, b) => a.name.localeCompare(b.name))

        for (const mFile of mFiles) {
          const fullPath = path.join(memberDir, mFile.name)
          const mName = path.basename(mFile.name, '.md')
          const mem = buildMemberSchema(fullPath, unitName, className, mName, cat)
          classSchema[fieldKey].push(mem)
        }
      }

      if (category === 'Interfaces' || kind === 'Interface') {
        unitSchema.interfaces.push(classSchema)
      } else {
        unitSchema.classes.push(classSchema)
      }
    }

    units.push(unitSchema)
  }

  // Generate api.json
  const apiRootSchema: ApiRootSchema = {
    $schema: 'https://graphics32.github.io/api.json',
    title: 'Graphics32 API Schema',
    description: 'Complete public API schema for Graphics32 exported from frontmatter YAML data',
    version: '1.0.0',
    units
  }

  fs.mkdirSync(outputDir, { recursive: true })
  fs.writeFileSync(path.join(outputDir, 'api.json'), JSON.stringify(apiRootSchema, null, 2), 'utf-8')

  // Generate llms.txt
  const llmsTxtLines: string[] = [
    '# Graphics32',
    '',
    '> High-performance 32-bit graphics library for Delphi and Lazarus/FPC',
    '',
    'Graphics32 is a fast 32-bit graphics library optimized for Delphi and Lazarus/FPC. It provides high-speed 32-bit ARGB bitmap handling, pixel blending modes, resampling kernels, vector polygon rasterization, layer management, and hardware/CPU acceleration (SSE2/AVX2).',
    '',
    '## Documentation',
    '',
    '- [Getting Started](/guide/): Overview of Graphics32 features, system requirements, and installation.',
    '- [Tutorials](/guide/tutorial/): Step-by-step programming guides and code examples.',
    '- [Alpha Composition](/guide/alpha-composition): Core concepts of 32-bit ARGB alpha blending and color math.',
    '- [Resampling and Transforms](/guide/resampling-and-transforms): Affine transformations, resampling filters, and spatial samplers.',
    '- [Drawing and Blending](/guide/drawing-and-blending): Canvas primitives, custom blend modes, and pixel operations.',
    '- [Vectorial Polygon Rasterizer](/guide/vpr): High-performance vector rasterization and anti-aliasing.',
    '- [Color Gradients](/guide/color-gradients): Linear, radial, and arbitrary multi-point color gradient fills.',
    '- [Using TImage32](/guide/using-timage32/overview): Visual components, image display, paint stages, and layer management.',
    '- [CPU Feature Detection](/guide/cpu-feature-detection): Runtime CPU capabilities detection and SIMD optimization.',
    '- [Examples](/examples/): Interactive example application projects.',
    '',
    '## API Reference',
    ''
  ]

  for (const u of units) {
    const sum = u.summary ? `: ${u.summary}` : ''
    llmsTxtLines.push(`- [${u.name}](${u.link})${sum}`)
  }

  llmsTxtLines.push(
    '',
    '## Machine-Readable Resources',
    '',
    '- [Full API Documentation](/llms-full.txt): Complete concatenated Markdown documentation containing all unit/class summaries and method signatures for AI context windows.',
    '- [API JSON Schema](/api.json): Complete public API schema exported from frontmatter YAML data.'
  )

  fs.writeFileSync(path.join(outputDir, 'llms.txt'), llmsTxtLines.join('\n'), 'utf-8')

  // Generate llms-full.txt
  const fullLines: string[] = [
    '# Graphics32 API Documentation (Full Context)',
    '',
    '> High-performance 32-bit graphics library for Delphi and Lazarus/FPC',
    '',
    'This document contains the complete API reference for Graphics32, including units, classes, interfaces, types, routines, constants, variables, and member signatures.',
    ''
  ]

  for (const u of units) {
    fullLines.push(`## Unit: ${u.name}`)
    fullLines.push(`- **URL**: ${u.link}`)
    if (u.summary) fullLines.push(`- **Summary**: ${u.summary}`)
    fullLines.push('')

    // Classes & Interfaces
    const items = [...u.classes, ...u.interfaces]
    if (items.length > 0) {
      fullLines.push('### Classes & Interfaces')
      fullLines.push('')
      for (const cls of items) {
        fullLines.push(`#### ${cls.name}`)
        fullLines.push(`- **URL**: ${cls.link}`)
        fullLines.push(`- **Kind**: ${cls.kind}`)
        if (cls.inheritance && cls.inheritance.length > 0) {
          fullLines.push(`- **Inheritance**: ${cls.inheritance.join(' -> ')}`)
        }
        if (cls.declaration) {
          fullLines.push(`- **Declaration**: \`${cls.declaration}\``)
        }
        if (cls.summary) {
          fullLines.push(`- **Summary**: ${cls.summary}`)
        }

        const memberGroups: { label: string; list: ApiMemberSchema[] }[] = [
          { label: 'Constructors', list: cls.constructors },
          { label: 'Methods', list: cls.methods },
          { label: 'Properties', list: cls.properties },
          { label: 'Events', list: cls.events },
          { label: 'Fields', list: cls.fields }
        ]

        for (const { label, list } of memberGroups) {
          if (list.length === 0) continue
          fullLines.push('')
          fullLines.push(`##### ${label}`)
          for (const m of list) {
            let decl = m.declaration ? `\`${m.declaration}\`` : `\`${m.name}\``
            let scopeStr = m.scope ? ` (${m.scope})` : ''
            fullLines.push(`- **${m.name}**${scopeStr}: ${decl}`)
            if (m.summary) fullLines.push(`  - Summary: ${m.summary}`)
            if (m.overloads && m.overloads.length > 0) {
              fullLines.push('  - Overloads:')
              for (const ov of m.overloads) {
                if (ov.signature) {
                  const ovSum = ov.summary ? ` — ${ov.summary}` : ''
                  fullLines.push(`    - \`${ov.signature}\`${ovSum}`)
                }
              }
            }
            fullLines.push(`  - URL: ${m.link}`)
          }
        }
        fullLines.push('')
      }
    }

    // Routines
    if (u.routines.length > 0) {
      fullLines.push('### Routines')
      fullLines.push('')
      for (const r of u.routines) {
        let decl = r.declaration ? `\`${r.declaration}\`` : `\`${r.name}\``
        fullLines.push(`#### ${r.name}`)
        fullLines.push(`- **URL**: ${r.link}`)
        fullLines.push(`- **Kind**: ${r.kind}`)
        fullLines.push(`- **Declaration**: ${decl}`)
        if (r.summary) fullLines.push(`- **Summary**: ${r.summary}`)
        if (r.overloads && r.overloads.length > 0) {
          fullLines.push('- **Overloads**:')
          for (const ov of r.overloads) {
            if (ov.signature) {
              const ovSum = ov.summary ? ` — ${ov.summary}` : ''
              fullLines.push(`  - \`${ov.signature}\`${ovSum}`)
            }
          }
        }
        fullLines.push('')
      }
    }

    // Types
    if (u.types.length > 0) {
      fullLines.push('### Types')
      fullLines.push('')
      for (const t of u.types) {
        let decl = t.declaration ? `\`${t.declaration}\`` : `\`${t.name}\``
        fullLines.push(`#### ${t.name}`)
        fullLines.push(`- **URL**: ${t.link}`)
        fullLines.push(`- **Declaration**: ${decl}`)
        if (t.summary) fullLines.push(`- **Summary**: ${t.summary}`)
        fullLines.push('')
      }
    }

    // Constants
    if (u.constants.length > 0) {
      fullLines.push('### Constants')
      fullLines.push('')
      for (const c of u.constants) {
        let decl = c.declaration ? `\`${c.declaration}\`` : `\`${c.name}\``
        fullLines.push(`#### ${c.name}`)
        fullLines.push(`- **URL**: ${c.link}`)
        fullLines.push(`- **Declaration**: ${decl}`)
        if (c.summary) fullLines.push(`- **Summary**: ${c.summary}`)
        fullLines.push('')
      }
    }

    // Variables
    if (u.variables.length > 0) {
      fullLines.push('### Variables')
      fullLines.push('')
      for (const v of u.variables) {
        let decl = v.declaration ? `\`${v.declaration}\`` : `\`${v.name}\``
        fullLines.push(`#### ${v.name}`)
        fullLines.push(`- **URL**: ${v.link}`)
        fullLines.push(`- **Declaration**: ${decl}`)
        if (v.summary) fullLines.push(`- **Summary**: ${v.summary}`)
        fullLines.push('')
      }
    }
  }

  fs.writeFileSync(path.join(outputDir, 'llms-full.txt'), fullLines.join('\n'), 'utf-8')
}

import fs from 'fs'
import path from 'path'

export interface SidebarItem {
  text: string
  link?: string
  items?: SidebarItem[]
  collapsed?: boolean
}

/**
  Extracts the page title from frontmatter or heading in Markdown file and strips 'Unit ' prefix for sidebar
 */
function getTitleFromFile(filePath: string, fallback: string): string {
  let title = fallback
  try {
    const content = fs.readFileSync(filePath, 'utf-8')
    const match = content.match(/^---\r?\n([\s\S]*?)\r?\n---/)
    if (match) {
      const frontmatter = match[1]
      const titleMatch = frontmatter.match(/^title:\s*["']?(.*?)["']?$/m)
      if (titleMatch && titleMatch[1]) {
        title = titleMatch[1].trim()
      }
    } else {
      const h1Match = content.match(/^#\s+(.+)$/m)
      if (h1Match && h1Match[1]) {
        title = h1Match[1].trim()
      }
    }
  } catch (e) {
    // fallback
  }

  // Strip 'Unit ' prefix if present for clean sidebar display
  if (title.startsWith('Unit ')) {
    title = title.substring(5).trim()
  }

  return title
}

/**
  Recursively generates VitePress sidebar configuration for a directory.
 */
export function generateSidebarForDir(
  rootDir: string,
  relPath: string = '',
  options: { collapsed?: boolean } = {}
): SidebarItem[] {
  const absPath = path.join(rootDir, relPath)
  if (!fs.existsSync(absPath)) return []

  const entries = fs.readdirSync(absPath, { withFileTypes: true })

  // Sort entries: directories first, then files
  const folders = entries
    .filter(e => e.isDirectory() && !e.name.startsWith('.'))
    .sort((a, b) => a.name.localeCompare(b.name, undefined, { numeric: true, sensitivity: 'base' }))

  const files = entries
    .filter(e => e.isFile() && e.name.endsWith('.md') && !e.name.startsWith('.'))
    .sort((a, b) => a.name.localeCompare(b.name, undefined, { numeric: true, sensitivity: 'base' }))

  const items: SidebarItem[] = []

  for (const folder of folders) {
    const subRel = path.join(relPath, folder.name)
    const indexMd = path.join(rootDir, subRel, 'index.md')
    const title = fs.existsSync(indexMd) ? getTitleFromFile(indexMd, folder.name) : folder.name
    const subItems = generateSidebarForDir(rootDir, subRel, options)

    // Strip organizational category folders from folder index links to match clean rewrites
    const cleanSubRel = subRel
      .split(/[/\\]/)
      .filter(segment => !['Classes', 'Types', 'Routines', 'Constants', 'Variables', 'Interfaces', 'Constructors', 'Methods', 'Properties', 'Events'].includes(segment))
      .join('/')

    const folderLink = fs.existsSync(indexMd)
      ? '/' + path.join(path.basename(rootDir), cleanSubRel, '/').replace(/\\/g, '/').replace(/\/+/g, '/')
      : undefined

    const entry: SidebarItem = {
      text: title,
      collapsed: options.collapsed ?? true
    }

    if (folderLink) {
      entry.link = folderLink
    }

    if (subItems.length > 0) {
      entry.items = subItems
    }

    items.push(entry)
  }

  for (const file of files) {
    if (file.name === 'index.md') continue
    const nameNoExt = file.name.slice(0, -3)
    const fileAbs = path.join(absPath, file.name)
    const title = getTitleFromFile(fileAbs, nameNoExt)

    // Strip organizational category folders from sidebar links to match clean rewrites
    const cleanRelPath = relPath
      .split(/[/\\]/)
      .filter(segment => !['Classes', 'Types', 'Routines', 'Constants', 'Variables', 'Interfaces', 'Constructors', 'Methods', 'Properties', 'Events'].includes(segment))
      .join('/')

    const parentDirName = path.basename(path.dirname(fileAbs))
    const isMemberIndex = nameNoExt.toLowerCase() === 'index' && ['Constructors', 'Methods', 'Properties', 'Events'].includes(parentDirName)
    const targetName = isMemberIndex ? `${parentDirName}-${nameNoExt}` : nameNoExt

    const link = '/' + path.join(path.basename(rootDir), cleanRelPath, targetName).replace(/\\/g, '/').replace(/\/+/g, '/')
    items.push({ text: title, link })
  }

  return items
}

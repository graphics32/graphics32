import { showInherited, showProtected, showAbstract } from './apiFilterState'
import memberDataRaw from './memberData.json'

const memberData = memberDataRaw as {
  byLink: Record<string, { isVirtual: boolean; isProtected: boolean; isAbstract?: boolean }>
}

function normalizePath(path: string): string {
  if (!path) return ''
  let cleaned = path.split('?')[0].split('#')[0]
  if (cleaned.endsWith('.html')) {
    cleaned = cleaned.slice(0, -5)
  }
  if (cleaned.length > 1 && cleaned.endsWith('/')) {
    cleaned = cleaned.slice(0, -1)
  }
  return cleaned
}

export function applySidebarFilter() {
  if (typeof window === 'undefined') return

  const links = document.querySelectorAll('.VPSidebar a.VPLink, .VPSidebar a.link, nav.VPSidebar a')

  links.forEach((a) => {
    const rawHref = a.getAttribute('href') || ''
    const linkPath = normalizePath(rawHref)
    const info = memberData.byLink[linkPath]

    const itemWrapper = a.closest('.VPSidebarItem') || a.parentElement
    if (!itemWrapper) return

    if (info) {
      let shouldHide = false
      if (info.isVirtual && !showInherited.value) {
        shouldHide = true
      }
      if (info.isProtected && !showProtected.value) {
        shouldHide = true
      }
      if (info.isAbstract && !showAbstract.value) {
        shouldHide = true
      }

      if (shouldHide) {
        (itemWrapper as HTMLElement).style.display = 'none'
      } else {
        (itemWrapper as HTMLElement).style.display = ''
      }
    }
  })

  // Hide empty category folders / groups in sidebar
  const groups = document.querySelectorAll('.VPSidebar .VPSidebarItem.has-children, .VPSidebar .items > .item')
  groups.forEach((group) => {
    const subItemsContainer = group.querySelector('.items')
    if (subItemsContainer) {
      const childItems = Array.from(subItemsContainer.children)
      if (childItems.length > 0) {
        const visibleChildren = childItems.filter((el) => (el as HTMLElement).style.display !== 'none')
        if (visibleChildren.length === 0) {
          (group as HTMLElement).style.display = 'none'
        } else {
          (group as HTMLElement).style.display = ''
        }
      }
    }
  })
}

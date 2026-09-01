<script setup lang="ts">
import { useData } from 'vitepress'
import { computed } from 'vue'
import { showInherited, showProtected, showAbstract } from '../apiFilterState'
import memberDataRaw from '../memberData.json'

const props = defineProps<{
  type?: string
  cls?: string
}>()

const { frontmatter } = useData()

interface MemberInfo {
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
  isAbstract?: boolean
  propertyType?: string
  link: string
}

interface ClassMembersData {
  Constructors: MemberInfo[]
  Methods: MemberInfo[]
  Properties: MemberInfo[]
  Events: MemberInfo[]
}

interface UnitMembersData {
  Classes: MemberInfo[]
  Interfaces: MemberInfo[]
  Types: MemberInfo[]
  Routines: MemberInfo[]
  Constants: MemberInfo[]
  Variables: MemberInfo[]
}

const memberData = memberDataRaw as {
  byLink: Record<string, MemberInfo>
  byClass: Record<string, ClassMembersData>
  byUnit?: Record<string, UnitMembersData>
}

const targetClassName = computed(() => {
  if (props.cls) return props.cls
  const fm = frontmatter.value
  if (fm?.parent) return fm.parent
  if (fm?.entity) {
    if (fm.entity.includes('.')) return fm.entity.split('.')[0]
    if (memberData.byClass[fm.entity]) return fm.entity
  }
  if (fm?.title && memberData.byClass[fm.title]) return fm.title
  return ''
})

const targetUnitName = computed(() => {
  const fm = frontmatter.value
  if (fm?.unit) return fm.unit
  if (fm?.title && !targetClassName.value) return fm.title.replace(/^Unit\s+/, '')
  return ''
})

const isUnitContext = computed(() => {
  return !targetClassName.value && !!targetUnitName.value
})

const classInfo = computed<ClassMembersData | undefined>(() => {
  return memberData.byClass[targetClassName.value]
})

const unitInfo = computed<UnitMembersData | undefined>(() => {
  return memberData.byUnit ? memberData.byUnit[targetUnitName.value] : undefined
})

function filterMembers(list: MemberInfo[] = []): MemberInfo[] {
  return list.filter((m) => {
    if (m.isVirtual && !showInherited.value) return false
    if (m.isProtected && !showProtected.value) return false
    if (m.isAbstract && !showAbstract.value) return false
    return true
  })
}

const activeCategories = computed(() => {
  const requestedType = (props.type || 'all').toLowerCase().trim()

  if (isUnitContext.value && unitInfo.value) {
    const categories = [
      { key: 'Classes', label: 'Classes', items: unitInfo.value.Classes || [] },
      { key: 'Interfaces', label: 'Interfaces', items: unitInfo.value.Interfaces || [] },
      { key: 'Types', label: 'Types', items: unitInfo.value.Types || [] },
      { key: 'Routines', label: 'Functions & Routines', items: unitInfo.value.Routines || [] },
      { key: 'Constants', label: 'Constants', items: unitInfo.value.Constants || [] },
      { key: 'Variables', label: 'Variables', items: unitInfo.value.Variables || [] }
    ]

    return categories
      .filter((cat) => {
        if (requestedType === 'all' || requestedType === 'members') return true
        return cat.key.toLowerCase() === requestedType || cat.key.toLowerCase().slice(0, -1) === requestedType
      })
      .map((cat) => ({
        ...cat,
        filteredItems: filterMembers(cat.items)
      }))
      .filter((cat) => cat.filteredItems.length > 0)
  }

  if (classInfo.value) {
    const categories = [
      { key: 'Constructors', label: 'Constructors', items: classInfo.value.Constructors || [] },
      { key: 'Methods', label: 'Methods', items: classInfo.value.Methods || [] },
      { key: 'Properties', label: 'Properties', items: classInfo.value.Properties || [] },
      { key: 'Events', label: 'Events', items: classInfo.value.Events || [] }
    ]

    return categories
      .filter((cat) => {
        if (requestedType === 'all' || requestedType === 'members') return true
        return cat.key.toLowerCase() === requestedType || cat.key.toLowerCase().slice(0, -1) === requestedType
      })
      .map((cat) => ({
        ...cat,
        filteredItems: filterMembers(cat.items)
      }))
      .filter((cat) => cat.filteredItems.length > 0)
  }

  return []
})

function renderInlineMarkdown(text: string | undefined | null): string {
  if (!text) return ''
  let html = text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')

  html = html.replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2">$1</a>')
  html = html.replace(/`([^`]+)`/g, '<code>$1</code>')
  return html
}
</script>

<template>
  <div class="api-members-container" v-if="activeCategories.length > 0">
    <div
      v-for="cat in activeCategories"
      :key="cat.key"
       class="api-member-category"
    >
      <h2 :id="cat.key.toLowerCase()">{{ cat.label }}</h2>

      <!-- Properties Table with Type & Scope -->
      <table v-if="cat.key === 'Properties'">
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Scope</th>
            <th>Description</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="item in cat.filteredItems" :key="item.name">
            <td>
              <a :href="item.link"><code>{{ item.name }}</code></a>
            </td>
            <td>
              <code v-if="item.propertyType">{{ item.propertyType }}</code>
              <span v-else>-</span>
            </td>
            <td>{{ item.scope }}</td>
            <td v-html="renderInlineMarkdown(item.summary)"></td>
          </tr>
        </tbody>
      </table>

      <!-- Constructors, Methods & Events Table -->
      <table v-else>
        <thead>
          <tr>
            <th>Name</th>
            <th>Description</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="item in cat.filteredItems" :key="item.name">
            <td>
              <a :href="item.link"><code>{{ item.name }}</code></a>
            </td>
            <td v-html="renderInlineMarkdown(item.summary)"></td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<style scoped>
.api-members-container {
  margin-top: 24px;
}

.api-member-category {
  margin-bottom: 32px;
}

.api-member-category h2 {
  font-size: 1.5rem;
  font-weight: 600;
  margin-top: 24px;
  margin-bottom: 16px;
  border-bottom: 1px solid var(--vp-c-divider);
  padding-bottom: 8px;
}

.api-member-category table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 12px;
}
</style>

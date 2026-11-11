import { ref } from 'vue'

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
  isAbstract?: boolean
  propertyType?: string
  link: string
}

export interface ClassMembersData {
  Constructors: MemberInfo[]
  Methods: MemberInfo[]
  Properties: MemberInfo[]
  Events: MemberInfo[]
}

export interface UnitMembersData {
  Classes: MemberInfo[]
  Interfaces: MemberInfo[]
  Types: MemberInfo[]
  Routines: MemberInfo[]
  Constants: MemberInfo[]
  Variables: MemberInfo[]
}

export interface MemberDataFile {
  byLink: Record<string, MemberInfo>
  byClass: Record<string, ClassMembersData>
  byUnit: Record<string, UnitMembersData>
}

const memberDataRef = ref<MemberDataFile | null>(null)
let fetchPromise: Promise<MemberDataFile | null> | null = null
const onLoadCallbacks: Array<() => void> = []

export function onMemberDataLoaded(callback: () => void) {
  if (memberDataRef.value) {
    callback()
  } else {
    onLoadCallbacks.push(callback)
  }
}

export function getMemberData(): MemberDataFile | null {
  if (memberDataRef.value) return memberDataRef.value
  if (typeof window === 'undefined') return null

  if (!fetchPromise) {
    const baseUrl = (import.meta.env.BASE_URL || '/').replace(/\/$/, '') + '/'
    const memberDataUrl = `${baseUrl}memberData.json`

    fetchPromise = fetch(memberDataUrl)
      .then((res) => {
        if (!res.ok) throw new Error(`Failed to fetch ${memberDataUrl}`)
        return res.json()
      })
      .then((data: MemberDataFile) => {
        memberDataRef.value = data
        onLoadCallbacks.forEach((cb) => cb())
        onLoadCallbacks.length = 0
        return data
      })
      .catch((err) => {
        console.warn('[memberDataLoader] Error loading memberData.json:', err)
        return null
      })
  }

  return memberDataRef.value
}

export { memberDataRef }

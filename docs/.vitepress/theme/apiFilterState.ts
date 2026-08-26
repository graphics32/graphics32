import { ref, watch } from 'vue'

const STORAGE_KEY_INHERITED = 'gr32_api_show_inherited'
const STORAGE_KEY_PROTECTED = 'gr32_api_show_protected'

// Default values: protected=false, inherited=true
export const showInherited = ref<boolean>(true)
export const showProtected = ref<boolean>(false)

// Initialize state from localStorage in browser environment
if (typeof window !== 'undefined') {
  try {
    const storedInherited = localStorage.getItem(STORAGE_KEY_INHERITED)
    if (storedInherited !== null) {
      showInherited.value = storedInherited === 'true'
    }
    const storedProtected = localStorage.getItem(STORAGE_KEY_PROTECTED)
    if (storedProtected !== null) {
      showProtected.value = storedProtected === 'true'
    }
  } catch (e) {
    // ignore
  }

  watch(showInherited, (val) => {
    try {
      localStorage.setItem(STORAGE_KEY_INHERITED, String(val))
    } catch (e) {
      // ignore
    }
  })

  watch(showProtected, (val) => {
    try {
      localStorage.setItem(STORAGE_KEY_PROTECTED, String(val))
    } catch (e) {
      // ignore
    }
  })
}

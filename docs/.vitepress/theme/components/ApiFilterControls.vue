<script setup lang="ts">
import { showInherited, showProtected } from '../apiFilterState'
import { useData } from 'vitepress'
import { computed } from 'vue'

const { frontmatter } = useData()

// Only display controls when on an API page
const isApiPage = computed(() => {
  return frontmatter.value?.docType === 'api' || !!frontmatter.value?.unit
})
</script>

<template>
  <div v-if="isApiPage" class="api-filter-controls">
    <div class="filter-header">Member Filters</div>
    <div class="filter-option">
      <label class="toggle-switch">
        <input type="checkbox" v-model="showInherited" />
        <span class="slider"></span>
      </label>
      <span class="label-text">Show Inherited</span>
    </div>
    <div class="filter-option">
      <label class="toggle-switch">
        <input type="checkbox" v-model="showProtected" />
        <span class="slider"></span>
      </label>
      <span class="label-text">Show Protected</span>
    </div>
  </div>
</template>

<style scoped>
.api-filter-controls {
  margin-bottom: 16px;
  padding: 12px;
  background-color: var(--vp-c-bg-soft);
  border: 1px solid var(--vp-c-divider);
  border-radius: 8px;
}

.filter-header {
  font-size: 0.85rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: var(--vp-c-text-2);
  margin-bottom: 10px;
}

.filter-option {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 8px;
  font-size: 0.88rem;
  color: var(--vp-c-text-1);
}

.filter-option:last-child {
  margin-bottom: 0;
}

.label-text {
  user-select: none;
  font-weight: 500;
}

/* Toggle Switch Styling */
.toggle-switch {
  position: relative;
  display: inline-block;
  width: 34px;
  height: 18px;
  flex-shrink: 0;
}

.toggle-switch input {
  opacity: 0;
  width: 0;
  height: 0;
}

.slider {
  position: absolute;
  cursor: pointer;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: var(--vp-c-neutral);
  transition: 0.2s;
  border-radius: 18px;
}

.slider:before {
  position: absolute;
  content: "";
  height: 14px;
  width: 14px;
  left: 2px;
  bottom: 2px;
  background-color: var(--vp-c-bg);
  transition: 0.2s;
  border-radius: 50%;
}

input:checked + .slider {
  background-color: var(--vp-c-brand-1);
}

input:checked + .slider:before {
  transform: translateX(16px);
}
</style>

<template>
  <div v-if="buildInfo" class="build-footer">
    <p>
      Generated from <code>{{ buildInfo.branch }}/{{ buildInfo.commit }}</code> on {{ buildInfo.timestamp }}
    </p>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { useData } from 'vitepress'

const { theme } = useData()
const buildInfo = computed(() => theme.value?.buildInfo)
</script>

<style scoped>
.build-footer {
  text-align: center;
  padding: 1rem 0;
  border-top: 1px solid var(--vp-c-divider);
  color: var(--vp-c-text-2);
  font-size: 0.85rem;
  margin-top: 2rem;
}

.build-footer code {
  font-family: var(--vp-font-family-mono);
  background-color: var(--vp-c-bg-soft);
  padding: 2px 5px;
  border-radius: 4px;
}
</style>

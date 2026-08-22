<script setup lang="ts">
import { useData } from 'vitepress'
import { computed } from 'vue'

const { page, frontmatter } = useData()

const declarationsText = computed(() => {
  if (frontmatter.value?.overloads?.length) {
    return frontmatter.value.overloads.map((ov: any) => ov.signature).join('\n')
  }
  return frontmatter.value?.declaration || ''
})
</script>

<template>
  <div class="vp-doc api-page-header">
    <!-- API Header Badge & Title -->
    <header class="api-header">
      <div class="api-breadcrumbs" v-if="frontmatter.unit">
        <span class="breadcrumb-item">
          <a :href="`/api/${frontmatter.unit}/`" class="unit-link">{{ frontmatter.unit }}</a>
        </span>
        <span class="sep" v-if="frontmatter.parent">&gt;</span>
        <span class="breadcrumb-item" v-if="frontmatter.parent">
          <a :href="`/api/${frontmatter.unit}/${frontmatter.parent}/`" class="parent-link">{{ frontmatter.parent }}</a>
        </span>
        <span class="sep">&gt;</span>
        <span class="breadcrumb-current">{{ frontmatter.entity ? frontmatter.entity.split('.').pop() : page.title }}</span>
        <span class="kind-badge" v-if="frontmatter.kind">{{ frontmatter.kind }}</span>
      </div>
      <h1 class="api-title" v-if="frontmatter.entity">{{ frontmatter.entity }}</h1>
      <p class="api-summary" v-if="frontmatter.summary">{{ frontmatter.summary }}</p>
    </header>

    <!-- Inheritance Breadcrumbs -->
    <div class="api-inheritance" v-if="frontmatter.inheritance && frontmatter.inheritance.length">
      <span class="inheritance-label">Inheritance:</span>
      <span
        v-for="(item, index) in frontmatter.inheritance"
        :key="item"
        class="inheritance-item"
      >
        <code>{{ item }}</code>
        <span v-if="index < frontmatter.inheritance.length - 1" class="sep"> &gt; </span>
      </span>
    </div>

    <!-- OVERLOADED METHOD/ROUTINE DETAILS -->
    <template v-if="frontmatter.overloads && frontmatter.overloads.length">
      <!-- Declarations List -->
      <section class="api-declaration-section">
        <h2>Declarations</h2>
        <div class="language-pascal vp-adaptive-theme">
          <button title="Copy Code" class="copy"></button>
          <span class="lang">pascal</span>
          <pre class="shiki"><code>{{ declarationsText }}</code></pre>
        </div>
      </section>

      <!-- Overload Details (Collapsible) -->
      <details class="details custom-block api-overloads-section">
        <summary>Overload Details</summary>
        <div v-for="(ov, idx) in frontmatter.overloads" :key="idx" class="overload-block">
          <h3>Overload {{ idx + 1 }}</h3>
          <div class="language-pascal vp-adaptive-theme">
            <span class="lang">pascal</span>
            <pre class="shiki"><code>{{ ov.signature }}</code></pre>
          </div>
          <p class="overload-summary" v-if="ov.summary">{{ ov.summary }}</p>

          <table v-if="ov.parameters && ov.parameters.length">
            <thead>
              <tr>
                <th>Parameter</th>
                <th>Type</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="param in ov.parameters" :key="param.name">
                <td><code>{{ param.name }}</code></td>
                <td><code>{{ param.type }}</code></td>
                <td>{{ param.description }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </details>
    </template>

    <!-- SINGLE SIGNATURE DETAILS -->
    <template v-else>
      <!-- Declaration Block -->
      <section class="api-declaration-section" v-if="frontmatter.declaration">
        <h2>Declaration</h2>
        <div class="language-pascal vp-adaptive-theme">
          <button title="Copy Code" class="copy"></button>
          <span class="lang">pascal</span>
          <pre class="shiki"><code>{{ declarationsText }}</code></pre>
        </div>
      </section>

      <!-- Parameters Table -->
      <section class="api-parameters-section" v-if="frontmatter.parameters && frontmatter.parameters.length">
        <h2>Parameters</h2>
        <table>
          <thead>
            <tr>
              <th>Parameter</th>
              <th>Type</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="param in frontmatter.parameters" :key="param.name">
              <td><code>{{ param.name }}</code></td>
              <td><code>{{ param.type }}</code></td>
              <td>{{ param.description }}</td>
            </tr>
          </tbody>
        </table>
      </section>
    </template>
  </div>
</template>

<style scoped>
.api-page-header {
  margin-bottom: 24px;
}

.api-breadcrumbs {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 0.9em;
  margin-bottom: 8px;
  flex-wrap: wrap;
}

.unit-link, .parent-link {
  color: var(--vp-c-brand-1);
  font-weight: 600;
  text-decoration: none;
}

.unit-link:hover, .parent-link:hover {
  text-decoration: underline;
}

.breadcrumb-current {
  font-weight: 600;
  color: var(--vp-c-text-1);
}

.kind-badge {
  background-color: var(--vp-c-bg-soft);
  color: var(--vp-c-text-1);
  border: 1px solid var(--vp-c-divider);
  padding: 2px 8px;
  border-radius: 12px;
  font-size: 0.8em;
  font-weight: 600;
}

.api-title {
  font-size: 2.2em;
  font-weight: 700;
  margin-top: 4px;
  margin-bottom: 12px;
  border: none;
}

.api-summary {
  font-size: 1.1em;
  color: var(--vp-c-text-2);
  margin-bottom: 16px;
}

.api-inheritance {
  background-color: var(--vp-c-bg-alt);
  padding: 8px 16px;
  border-radius: 8px;
  border: 1px solid var(--vp-c-divider);
  margin-bottom: 24px;
  font-size: 0.9em;
}

.inheritance-label {
  font-weight: 600;
  margin-right: 8px;
  color: var(--vp-c-text-2);
}

.sep {
  color: var(--vp-c-text-3);
}

.api-declaration-section, .api-parameters-section, .api-overloads-section {
  margin-bottom: 24px;
}

.overload-block {
  margin-bottom: 32px;
  padding-bottom: 16px;
  border-bottom: 1px dashed var(--vp-c-divider);
}

.overload-block:last-child {
  border-bottom: none;
}

.overload-summary {
  font-style: italic;
  margin-top: 8px;
  margin-bottom: 12px;
  color: var(--vp-c-text-2);
}
</style>

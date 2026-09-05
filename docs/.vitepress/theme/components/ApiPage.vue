<script setup lang="ts">
import { useData } from 'vitepress'

const { page, frontmatter } = useData()

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
        <span class="scope-badge" v-if="frontmatter.scope" :class="`scope-${frontmatter.scope.toLowerCase()}`">{{ frontmatter.scope }}</span>
      </div>

      <!-- Inherited Member Banner -->
      <div class="inherited-from-banner" v-if="frontmatter.inheritedFrom">
        <span class="inherited-label">Inherited from:</span>
        <code>{{ frontmatter.inheritedFrom }}</code>
      </div>

      <h1 class="api-title" v-if="frontmatter.entity">{{ frontmatter.entity }}</h1>
      <p class="api-summary" v-if="frontmatter.summary" v-html="renderInlineMarkdown(frontmatter.summary)"></p>
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

.scope-badge {
  padding: 2px 8px;
  border-radius: 12px;
  font-size: 0.8em;
  font-weight: 600;
  border: 1px solid var(--vp-c-divider);
}

.scope-protected {
  background-color: var(--vp-c-yellow-soft, #fef3c7);
  color: var(--vp-c-yellow-dark, #b45309);
  border-color: var(--vp-c-yellow-dim, #fde68a);
}

.scope-public {
  background-color: var(--vp-c-green-soft, #d1fae5);
  color: var(--vp-c-green-dark, #047857);
  border-color: var(--vp-c-green-dim, #a7f3d0);
}

.scope-published {
  background-color: var(--vp-c-brand-soft, #e0e7ff);
  color: var(--vp-c-brand-1, #4338ca);
  border-color: var(--vp-c-brand-dim, #c7d2fe);
}

.inherited-from-banner {
  background-color: var(--vp-c-bg-soft);
  border: 1px solid var(--vp-c-brand-1);
  color: var(--vp-c-text-1);
  padding: 6px 12px;
  border-radius: 6px;
  font-size: 0.85em;
  margin-top: 6px;
  margin-bottom: 10px;
  display: inline-block;
}

.inherited-label {
  font-weight: 600;
  margin-right: 6px;
  color: var(--vp-c-brand-1);
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
</style>

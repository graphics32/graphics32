<script setup lang="ts">
import { useData, useRoute } from 'vitepress'
import { computed, watchEffect, watch, onMounted } from 'vue'

const { page, frontmatter } = useData()
const route = useRoute()

const declarationsText = computed(() => {
  if (frontmatter.value?.overloads?.length) {
    return frontmatter.value.overloads.map((ov: any) => ov.signature).join('\n')
  }
  return frontmatter.value?.declaration || ''
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

function normalizedReturns(returns: any): Array<{ type: string; description: string }> {
  if (!returns) return []
  if (Array.isArray(returns)) return returns
  if (typeof returns === 'object') return [returns]
  return []
}

watchEffect(() => {
  if (!page.value) return
  const extraHeaders: any[] = []

  const hasOverloads = !!(frontmatter.value?.overloads && frontmatter.value.overloads.length)
  const hasDeclaration = !!(frontmatter.value?.declaration || hasOverloads)
  const hasParameters = !!(!hasOverloads && frontmatter.value?.parameters && frontmatter.value.parameters.length)
  const singleReturns = normalizedReturns(frontmatter.value?.returns)
  const hasReturns = !!(!hasOverloads && singleReturns.length)

  if (hasOverloads) {
    extraHeaders.push({
      level: 2,
      title: 'Declarations',
      slug: 'declarations',
      link: '#declarations'
    })
    extraHeaders.push({
      level: 2,
      title: 'Overload Details',
      slug: 'overload-details',
      link: '#overload-details'
    })
  } else if (hasDeclaration) {
    extraHeaders.push({
      level: 2,
      title: 'Declaration',
      slug: 'declaration',
      link: '#declaration'
    })
    if (hasParameters) {
      extraHeaders.push({
        level: 2,
        title: 'Parameters',
        slug: 'parameters',
        link: '#parameters'
      })
    }
    if (hasReturns) {
      extraHeaders.push({
        level: 2,
        title: 'Returns',
        slug: 'returns',
        link: '#returns'
      })
    }
  }

  const existingHeaders = page.value.headers || []
  const existingSlugs = new Set(existingHeaders.map((h: any) => h.slug))
  const newHeaders = extraHeaders.filter(h => !existingSlugs.has(h.slug))

  page.value.headers = [...newHeaders, ...existingHeaders]
})

watch(
  () => route.hash,
  (newHash) => {
    if (newHash === '#overload-details') {
      const el = document.querySelector('details.api-overloads-section') as HTMLDetailsElement | null
      if (el) el.open = true
    }
  },
  { immediate: true }
)

onMounted(() => {
  document.addEventListener('click', (e) => {
    const target = e.target as HTMLElement | null
    const anchor = target?.closest('a')
    if (anchor && anchor.getAttribute('href') === '#overload-details') {
      const el = document.querySelector('details.api-overloads-section') as HTMLDetailsElement | null
      if (el) el.open = true
    }
  })
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

    <!-- OVERLOADED METHOD/ROUTINE DETAILS -->
    <template v-if="frontmatter.overloads && frontmatter.overloads.length">
      <!-- Declarations List -->
      <section class="api-declaration-section">
        <h2 id="declarations" tabindex="-1">
          Declarations
          <a class="header-anchor" href="#declarations" aria-label="Permalink to &quot;Declarations&quot;">&#8203;</a>
        </h2>
        <div class="language-pascal vp-adaptive-theme">
          <button title="Copy Code" class="copy"></button>
          <span class="lang">pascal</span>
          <pre class="shiki"><code>{{ declarationsText }}</code></pre>
        </div>
      </section>

      <!-- Overload Details (Collapsible) -->
      <details class="details custom-block api-overloads-section">
        <summary>
          <h2 id="overload-details" tabindex="-1">Overload Details</h2>
        </summary>
        <div v-for="(ov, idx) in frontmatter.overloads" :key="idx" class="overload-block">
          <h3>Overload {{ idx + 1 }}</h3>
          <div class="language-pascal vp-adaptive-theme">
            <span class="lang">pascal</span>
            <pre class="shiki"><code>{{ ov.signature }}</code></pre>
          </div>
          <p class="overload-summary" v-if="ov.summary" v-html="renderInlineMarkdown(ov.summary)"></p>

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
                <td><code v-html="renderInlineMarkdown(param.name)"></code></td>
                <td><code v-html="renderInlineMarkdown(param.type)"></code></td>
                <td v-html="renderInlineMarkdown(param.description)"></td>
              </tr>
            </tbody>
          </table>

          <table v-if="ov.returns && normalizedReturns(ov.returns).length">
            <thead>
              <tr>
                <th>Type</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="(ret, rIdx) in normalizedReturns(ov.returns)" :key="rIdx">
                <td><code v-html="renderInlineMarkdown(ret.type)"></code></td>
                <td v-html="renderInlineMarkdown(ret.description)"></td>
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
        <h2 id="declaration" tabindex="-1">
          Declaration
          <a class="header-anchor" href="#declaration" aria-label="Permalink to &quot;Declaration&quot;">&#8203;</a>
        </h2>
        <div class="language-pascal vp-adaptive-theme">
          <button title="Copy Code" class="copy"></button>
          <span class="lang">pascal</span>
          <pre class="shiki"><code>{{ declarationsText }}</code></pre>
        </div>
      </section>

      <!-- Parameters Table -->
      <section class="api-parameters-section" v-if="frontmatter.parameters && frontmatter.parameters.length">
        <h2 id="parameters" tabindex="-1">
          Parameters
          <a class="header-anchor" href="#parameters" aria-label="Permalink to &quot;Parameters&quot;">&#8203;</a>
        </h2>
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
              <td><code v-html="renderInlineMarkdown(param.name)"></code></td>
              <td><code v-html="renderInlineMarkdown(param.type)"></code></td>
              <td v-html="renderInlineMarkdown(param.description)"></td>
            </tr>
          </tbody>
        </table>
      </section>

      <!-- Returns Table -->
      <section class="api-returns-section" v-if="frontmatter.returns && normalizedReturns(frontmatter.returns).length">
        <h2 id="returns" tabindex="-1">
          Returns
          <a class="header-anchor" href="#returns" aria-label="Permalink to &quot;Returns&quot;">&#8203;</a>
        </h2>
        <table>
          <thead>
            <tr>
              <th>Type</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(ret, rIdx) in normalizedReturns(frontmatter.returns)" :key="rIdx">
              <td><code v-html="renderInlineMarkdown(ret.type)"></code></td>
              <td v-html="renderInlineMarkdown(ret.description)"></td>
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

.api-declaration-section, .api-parameters-section, .api-returns-section, .api-overloads-section {
  margin-bottom: 24px;
}

.details.api-overloads-section summary h2 {
  display: inline;
  margin: 0;
  padding: 0;
  border: none;
  font-size: 1.05em;
  font-weight: 600;
  color: inherit;
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

# Documentation Generation and Maintenance Guide

This document outlines the workflow, architecture, and commands for generating, building, and maintaining the Graphics32 documentation site using VitePress. It serves as a guide for both human maintainers and AI coding agents.

For detailed instructions on API documentation extraction, generic type naming rules, and unit tracking checklists, see [API Documentation Generation Guide](./how_to_generate_API_documentation.md).

---

## 1. Overview & Architecture

The Graphics32 documentation is built as a static site using [VitePress](https://vitepress.dev/).

### Repository Layout
```
/
├── package.json                   # Node package config with VitePress dependencies & scripts
├── docs/
│   ├── .vitepress/
│   │   ├── config.mts             # VitePress configuration (navigation, sidebars, theme)
│   │   └── theme/
│   │       ├── index.ts           # Custom theme setup (@lando/vitepress-theme-default-plus, medium-zoom)
│   │       └── components/
│   │           └── ApiPage.vue    # Custom Vue Layout component for API reference pages
│   ├── index.md                   # Home / Landing page
│   ├── how_to_generate_documentation.md # This guide
│   ├── how_to_generate_API_documentation.md # Detailed API generation & tracking guide
│   ├── guide/                     # Conceptual guides, overview, topics, installation
│   ├── examples/                  # Code examples and tutorials
│   ├── public/                    # Favicon & public assets
│   │   ├── favicon.ico
│   │   ├── favicon.png
│   │   └── images/                # Embedded documentation images
│   └── api/                       # API reference documentation
│       └── <UnitName>/
│           ├── index.md           # Unit overview
│           └── <ClassName>/
│               ├── index.md       # Class overview
│               ├── Constructors/
│               │   └── <MethodName>.md # Grouped constructor overloads
│               ├── Methods/
│               │   └── <MethodName>.md # Grouped method overloads
│               ├── Properties/
│               │   └── <PropName>.md   # Property documentation
│               └── Events/
│                   └── <EventName>.md  # Event documentation
```

---

## 2. Prerequisites

- **Node.js**: Version 18.0.0 or higher (Node 22+ recommended)
- **npm**: Version 9.0.0 or higher

---

## 3. Initial Setup & Dependencies

To set up the documentation environment from scratch:

1. **Install Dependencies**:
   ```bash
   npm install
   ```

2. **Required Packages**:
   - `vitepress`
   - `@lando/vitepress-theme-default-plus`
   - `vitepress-plugin-mermaid`
   - `medium-zoom`
   - `markdown-it-mathjax3`

---

## 4. Running Local Development Server

To run the live preview server while editing documentation:

```bash
npm run docs:dev
```

This starts a hot-reloading dev server (typically at `http://localhost:5173`).

---

## 5. Building the Static Site

To generate production HTML/CSS/JS artifacts:

```bash
npm run docs:build
```

The output will be placed in `docs/.vitepress/dist`.

To preview the production build locally:

```bash
npm run docs:preview
```

---

## 6. How API Documentation is Generated and Organized

See the [API Documentation Generation Guide](./how_to_generate_API_documentation.md) for full extraction rules, filename sanitization, and the unit tracking checklist.

### Custom Vue Layout Component (`layout: api`)

All API reference pages use a custom Vue layout component (`ApiPage.vue` registered in `.vitepress/theme/index.ts`).

#### Why `layout: api`?
- **Enforces Fixed Structure**: Every API page shares identical header typography, unit badges, declaration blocks, inheritance trees, parameter tables, and sidebars.
- **Maintainer Freedom**: Human maintainers and AI agents write structured metadata in YAML frontmatter and focus on writing explanations, remarks, and code examples in Markdown below.

#### YAML Frontmatter Specification for API Pages:
```yaml
---
layout: api
unit: GR32
entity: TBitmap32
kind: Class # Class, Method, Property, Function, Record, Interface, Constant, Type
declaration: "TBitmap32 = class(TCustomBitmap32)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomBitmap32
summary: "Primary 32-bit ARGB bitmap container class in Graphics32."
parameters:
  - name: FillColor
    type: TColor32
    description: "The 32-bit ARGB color to fill the bitmap with."
---
```

### Generation Workflow Diagram

```mermaid
flowchart TD
 API.Source[[Source code]]
 API.comments(Source code comments)
 API.md[["API .md files<br>(in docs/api)"]]
 API.Struct(Source DOM)

 API.Consolidate{AI Agent}
 Jules.CreateDOM{AI Agent}
 Jules.ExtractComments{AI Agent}

 API.Source --> Jules.CreateDOM
 Jules.CreateDOM --> API.Struct

 API.Source --> Jules.ExtractComments
 Jules.ExtractComments --> API.comments

 API.Struct -.-> API.Consolidate
 API.md -.-> API.Consolidate
 API.comments -.-> API.Consolidate

 Output.API(API documentation)

 API.Consolidate --> Output.API

 Concepts.md[["Conceptual documentation .md files<br>(in docs/guide and docs/examples)"]]

 Site[Static documentation site]

 Site.Consolidate{Vitepress}

 Output.API ==> Site.Consolidate
 Concepts.md ==> Site.Consolidate
 Site.Consolidate ==> Site
```

---

## 7. Workflow for Updating Documentation

### A. Updating Conceptual Guides or Examples (Human Maintainers)
Human maintainers can directly edit or create `.md` files in `docs/guide/` or `docs/examples/`. Once committed, VitePress automatically includes them in the site build.

### B. Updating API Documentation when Pascal Source Code Changes
Follow the step-by-step procedures and progress checklist in [how_to_generate_API_documentation.md](./how_to_generate_API_documentation.md).

---

## 8. GitHub Actions Deployment & Staging Workflows

To prevent updating live documentation without human review, deployment to [https://github.com/graphics32/graphics32.github.io](https://github.com/graphics32/graphics32.github.io) should target a **`staging`** branch or staging preview environment.

### Strategy 1: Staging Branch Deployment (`publish_branch: staging`)

The following workflow builds the site on every push to `master`/`main` and deploys it to the `staging` branch of `graphics32.github.io`:

```yaml
name: Deploy Documentation to Staging

on:
  push:
    branches:
      - master
      - main

permissions:
  contents: write

jobs:
  deploy-docs-staging:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout Source Repository
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: 'npm'

      - name: Install Dependencies
        run: npm ci

      - name: Build VitePress Site
        run: npm run docs:build

      - name: Deploy to Staging Branch on graphics32.github.io
        uses: peaceiris/actions-gh-pages@v4
        with:
          personal_token: ${{ secrets.DOCS_DEPLOY_TOKEN }}
          external_repository: graphics32/graphics32.github.io
          publish_branch: staging
          publish_dir: docs/.vitepress/dist
          commit_message: "Automated staging build from graphics32 main repository"
```

---

## 9. Transitioning from Legacy Documentation Tools

- The legacy `DocProcessor/` tool and `Documentation/Source/` folder are preserved for reference during initial migration, but will be decommissioned once all content is transferred to VitePress.
- No external Pascal parsing executables or Windows-only CHM compilers are required to maintain or build this site.

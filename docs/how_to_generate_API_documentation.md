# API Documentation Generation & Maintenance Guide

This document contains comprehensive instructions for human maintainers and AI agents to author, update, and maintain the API documentation for the **Graphics32** library using **VitePress**. It is designed to be complete and self-contained so any maintainer or agent can create, update, and audit API documentation accurately without additional instructions.

---

## 1. Overview & Document Purpose

This guide details how Pascal source units in `Source/` are parsed and converted into VitePress Markdown pages in `docs/api/`.

It defines:
1. **Filename Sanitization Rules**: Safe cross-platform mapping for generic types (e.g. `TList<T>` $\rightarrow$ `TList(T).md`).
2. **Custom Vue Layout Architecture**: Separating structured machine data (YAML frontmatter) from human-editable Markdown body.
3. **Documentation Inheritance**: Virtual route member generation and inherited sidebar merging.
4. **Categories & Sub-categories Reference**: The complete hierarchy of unit member categories and sub-categories supported by VitePress configuration, plugins, and Vue components.
5. **Progress Checklist**: A flat tracking list of all Pascal units in `Source/`.

**Notes:**

- **Custom Vue Layout (`docs/.vitepress/theme/components/ApiPage.vue`)**: Structured YAML frontmatter for API pages is rendered in the `#doc-before` slot of `DefaultTheme.Layout`.
- **CSS Styling (`vp-doc`)**: All API page elements (`ApiPage.vue`) are wrapped in the `.vp-doc` class to inherit VitePress typography, table gridlines, and code block styling.

---

## 2. Unit Member Categories & Sub-Categories Reference

Based on `.vitepress/config.mts`, `.vitepress/generateMemberData.ts`, `.vitepress/sidebar.ts`, `.vitepress/virtualMembers.ts`, and `.vitepress/theme/components/ApiMembers.vue`, the documentation build system recognizes the following top-level unit member categories and member sub-categories:

### Supported Unit Member Categories
Top-level categories organizational folders directly under `docs/api/<UnitName>/`:
1. **Classes** (`/api/<UnitName>/Classes/`)
2. **Interfaces** (`/api/<UnitName>/Interfaces/`)
3. **Types** (`/api/<UnitName>/Types/`)
4. **Routines** (`/api/<UnitName>/Routines/`)
5. **Constants** (`/api/<UnitName>/Constants/`)
6. **Variables** (`/api/<UnitName>/Variables/`)

### Supported Member Sub-Categories
Sub-categories exist under individual container entities (such as individual `Classes`, `Interfaces`, or complex record structures):
1. **Constructors** (`Constructors/`) - Supported under `Classes`. **Note:** Destructors (e.g., `destroy`) are categorized under **Constructors** to group instance lifecycle methods together.
2. **Methods** (`Methods/`) - Supported under `Classes` and `Interfaces`.
3. **Properties** (`Properties/`) - Supported under `Classes` and `Interfaces`.
4. **Events** (`Events/`) - Supported under `Classes`.
4. **Fields** (`Fields/`) - Supported under `Classes` and `Records`.
5. **Operators** (`Operators/`) - Operator overloads (e.g. `operator Implicit`, `operator Add`) belong as a member sub-category under `Classes` or complex record types.

### Category to Frontmatter `kind` Mapping Table

The table below defines the mapping from organizational member categories to their corresponding YAML frontmatter `kind` values:

| Category / Sub-Category | Frontmatter `kind` Value | Description |
|---|---|---|
| **Classes** | `Class` | Class overview pages (`index.md`). |
| **Interfaces** | `Interface` | Interface overview pages (`index.md`). |
| **Types** | `Type` | Enums, aliases, sets, procedural pointers, and simple records. |
| **Routines** | `Function` or `Procedure` | Standalone unit functions and procedures. |
| **Constants** | `Constant` | Unit constants and typed constants. |
| **Variables** | `Variable` | Unit global variables and threadvars. |
| **Constructors** | `Constructor` | Constructors and destructors for classes/records. |
| **Methods** | `Method` | Member procedures and functions (including class methods). |
| **Properties** | `Property` | Member properties (including class properties). |
| **Events** | `Event` | Delegation event properties (e.g., `notify` events). |
| **Fields** | `Field` | Class fields (variables). |
| **Operators** | `Operator` | Overloaded record and class operators. |

### Public API Constructs Requiring Special Consideration
- **Record Types & Fields**: Simple record types (e.g., `TFixedRect`, `TFloatPoint`) are placed under `Types/`. Complex records containing methods, properties, or operator overloads may be grouped under `<UnitName>/Records/` or `<UnitName>/Types/`.
- **Destructors**: Destructors are documented under the **Constructors** sub-category (`Constructors/<Name>.md`) with `kind: Constructor`.
- **Operator Overloads**: Documented under the `Operators/` sub-category with `kind: Operator`.
- **Class Methods & Class Properties**: Documented under `Methods/` and `Properties/` with `class` included in the declaration signature.
- **Resourcestrings**: `resourcestrings` are intentional library internal/localized assets and are **omitted** from API documentation.

---

## 3. Generic Identifier, Directory & File Naming Rules

API documentation files reside under `docs/api/` matching unit, class, and member hierarchies:

```
docs/api/
  index.md                      # Overall API Overview
  <UnitName>/
    index.md                    # Unit Overview (e.g. docs/api/GR32/index.md)
    <ClassName>/
      index.md                  # Class Overview (e.g. docs/api/GR32/TBitmap32/index.md)
      Constructors/
        <MethodName>.md         # Constructor doc (e.g. docs/api/GR32/TBitmap32/Constructors/Create.md)
      Methods/
        <MethodName>.md         # Method doc (e.g. docs/api/GR32/TBitmap32/Methods/Draw.md)
      Properties/
        <PropertyName>.md       # Property doc (e.g. docs/api/GR32/TBitmap32/Properties/Pixel.md)
```


### A. The Naming Problem
Pascal generics and advanced types can contain angle brackets `<` and `>`. For example: `TList<T>`, `TDictionary<TKey, TValue>`.

- Angle brackets (`< >`) are **illegal file system characters** on Windows, macOS, and Linux.
- Replacing `< >` with underscores (`_`) creates **silent name collision risks** because `_` is a valid identifier character in Pascal (e.g., `TList_1` vs `TList<1>`).
- Replacing `< >` with square brackets (`[ ]`) conflicts with **VitePress / Vue Router dynamic route parameters** (where `[id].md` is treated as a dynamic parameter route).

### B. The `TList(T)` Parentheses Solution
To ensure 100% collision-free filenames that work across all operating systems without Vue Router conflicts:

1. **Filename Mapping**: Replace `<` with `(` and `>` with `)` in Markdown filenames:
   - `TList<T>` $\rightarrow$ `docs/api/GR32_Containers/TList(T).md`
   - `TDictionary<TKey, TValue>` $\rightarrow$ `docs/api/GR32_Containers/TDictionary(TKey,TValue).md`

2. **Display Name in Frontmatter**: Set the exact Pascal declaration name in YAML frontmatter:
   ```yaml
   ---
   layout: api
   unit: GR32_Containers
   entity: TList<T>
   kind: Class
   ---
   ```
   VitePress will display the exact formatted identifier `TList<T>` in page headers, search results, and sidebars, while the filesystem safely stores `TList(T).md`.

---

## 4. Frontmatter Schemas & Guidelines

All API pages must use `layout: doc` and `docType: api` in YAML frontmatter.

### Structure
- **YAML Frontmatter**: Machine-readable metadata (`unit`, `parent`, `entity`, `kind`, etc.).
- **Markdown Body**: Human-editable content (usage explanations, remarks, edge cases, code examples).


### Required & Optional Frontmatter Fields
| Field | Type | Description |
|---|---|---|
| `layout` | String | Must be `doc`. |
| `docType` | String | Must be `api`. |
| `unit` | String | Name of the unit (e.g., `GR32`). |
| `parent` | String | Optional. Name of parent class/record (e.g., `TBitmap32`). Enables 3-level breadcrumbs: `GR32 > TBitmap32 > Member`. |
| `entity` | String | Full entity identifier (e.g., `TBitmap32.Draw`). |
| `kind` | String | Entity classification (`Class`, `Method`, `Constructor`, `Property`, `Function`, `Type`, `Constant`, `Field`). |
| `scope` | String | Optional. Member visibility scope (`Public`, `Protected`, `Published`). Renders a styled scope badge in headers. |
| `abstract` | Boolean | Optional. Set to `true` for Category 1, 2, or 3 abstract classes (see: `abstract-classes.md`). Used by VitePress member filters to toggle abstract class visibility. |
| `summary` | String | High-level summary description. Keep short. Avoid details that are better described in the content. Often used in tables. |
| `declaration` | String | Pascal procedure/function/type signature for single-signature pages. |
| `inheritance` | Array | Required for Class entities. Inheritance chain, starting from base class (`TObject` is omitted) and ending in the class itself. |
| `parameters` | Array | Parameter list objects `[ { name, type, description } ]`. |
| `returns` | Array / Object | Return value object or list `[ { type, description } ]` for functions or methods returning a value. `description can contain Markdown (quoted or `|` literal). |
| `seealso` | Array / String | Optional. List of cross-references. Can contain symbolic links (e.g. `"[[GR32_VectorUtils]]"`) and other Markdown (quoted or `|` literal). Automatically rendered as a `## See also` bulleted list at the bottom of the page. |
| `overloads` | Array | Array of overload objects for overloaded methods/routines. Each overload entry contains `signature`, `summary`, `parameters`, and `returns`. |
| `inheritedFrom` | String | Optional. Full identifier of base class member if inherited (e.g., `TCustomBitmap32.Width`). |
| `aliases` | Array / String | Optional. List of additional symbol names mapped to this page for `[[symbol]]` resolution (e.g. `aliases: [clBlack32, clWhite32]`). |

---

### Schema A: Single Signature Page
````yaml
---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: DelaunayTriangulation
kind: Function
declaration: "function DelaunayTriangulation(Points: TArrayOfFloatPoint): TArrayOfTriangleVertexIndices;"
summary: "Generates a Delaunay triangulation mesh from a set of 2D floating-point points."
parameters:
  - name: Points
    type: TArrayOfFloatPoint
    description: "Array of 2D input points."
returns:
  - type: TArrayOfTriangleVertexIndices
    description: "An array of triangle index triplets."
seealso:
  - "[[ClipPolygon]]"
  - "[[GR32_VectorUtils]]"
  - "[Delaunay triangulation (Wikipedia)](https://en.wikipedia.org/wiki/Delaunay_triangulation)"
---

## Example

```pascal
var
  Bmp: TBitmap32;
begin
  // Create a 800x600 bitmap and fill it with the color red
  Bmp := TBitmap32.Create(800, 600);
  try
    Bmp.Clear(clRed32);
  finally
    Bmp.Free;
  end;
end;
```
````

---

### Schema B: Overloaded Method Page
When a method or function has multiple signatures with differing parameters, use the `overloads` array schema. This displays a grouped `Declarations` block at the top, followed by separate parameter tables for each overload:

```yaml
---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Draw
kind: Method
summary: "Draws a source bitmap or sub-rectangle onto this bitmap using current DrawMode and CombineMode."
overloads:
  - signature: "procedure Draw(DstX, DstY: Integer; Src: TCustomBitmap32); overload;"
    summary: "Draws the entire source bitmap at top-left pixel position (DstX, DstY)."
    parameters:
      - name: DstX, DstY
        type: Integer
        description: "Top-left destination coordinate on this bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap to draw."

  - signature: "procedure Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32); overload;"
    summary: "Stretches and blends a sub-rectangle from the source bitmap into a destination rectangle."
    parameters:
      - name: DstRect
        type: TRect
        description: "Target destination rectangle on this bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle on the source bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap to copy or blend pixels from."
---
```

---

## 5. How an AI Agent Populates Unit Members

To manage token limits effectively, member lists are populated **in small batches** when an agent begins work on a unit:

1. **Inspect Unit Source**: Read the `interface` section of `Source/<UnitName>.pas`.
2. **Expand the Unit Item**: Under `- [ ] <UnitName>`, insert nested checklist sections for Classes, Functions, Records, Interfaces, Constants, and Other Types.
3. **Check Off Completed Items**: Check off items (`- [x]`) as Markdown files are created.
4. **Mark Unit Complete**: Mark `- [x] <UnitName>` when all members are fully documented.

### Rules
- When tasked to document a class, do not include class members inherited from `TObject` or `TPersistent` (including `Destroy`, `Assign`, and `AssignTo`) unless instructed otherwise.
- Protected methods and properties that are promoted in a derived class must be documented on the base class. Apart from this, protected members are not documented unless instructed otherwise.
- Class methods that implement an interface member are not documented, unless the method is public.
  It is assumed that interface members are documented on the interface type.
- Metaclass types (`class of T`) are documented together with the concrete class type.
  It is often sufficient to show the metaclass declaration together with the class declaration, and add a symbol alias for the metaclass name:
  ```
  entity: TCustomBitmap32
  kind: Class
  aliases: [TCustomBitmap32Class])
  ```

- If the existing documentation is found to be incorrect, outdated or obsolete (e.g. a topic is no longer valid because the item it documents no longer exist), notify the user and ask for confirmation before fixing the problem.
- If an item in the unit list is marked "(document only at unit level)", then only an `index.md` file should be generated for that unit; The individual types, constants, or variables in the unit are not to be documented indivually.
- Do not edit the `docs/.vitepress/theme/memberData.json` file.
  The file is generated automatically by Vitepress at build and startup time and does not need to be kept up to date with other edits.

### Layout
- The Frontmatter `entity` value is automatically inserted as a `<h1>` header, at the top of the page. Do not add it manually in the markup.
- The Frontmatter `summary` value is automatically inserted just below the `entity` header..
- Other generated content is automatically inserted below `summary`.
- Normally, the first thing in the markup of an API page, is a `## Description` section.
- **Enumeration types** must be formatted as one table per enumeration type, one row per values.
  For example, for `TLogicalOperator`:
  | Value | Description |
  | --- | --- |
  | `loXOR` | Performs a bitwise Exclusive-OR (`xor`) operation between pixel colors and bitmask. |
  | `loAND` | Performs a bitwise AND (`and`) operation between pixel colors and bitmask. |
  | `loOR` | Performs a bitwise OR (`or`) operation between pixel colors and bitmask. |
- **Simple record types** can be formatted as one table per record type, one row per field.
  For example, for `TFixedPoint`:
  | Field | Type | Description |
  | --- | --- | --- |
  | `X` | TFixed | X-coordinate in fixed precision. |
  | `Y` | TFixed | Y-coordinate in fixed precision. |
- **Variant record types** can be formatted as one table for the invariant part (if any), and one table per variant.
  For example, for `TFixedRec` (no invariant part):
  **case 0**
  | Field | Type | Description |
  | --- | --- | --- |
  | `Fixed` | TFixed | Value in [16:16] fixed precision format. |

  **case 1**
  | Field | Type | Description |
  | --- | --- | --- |
  | `Frac` | SmallInt | Fractional part of fixed precision value. |
  | `Int` | SmallInt | Integer part of fixed precision value. |
- **Complex record members** are documented as classes with regard to methods and properties.
- **Pointer types** are generally documented along with the type they point to. For example:
  ```
  aliases: [PByteArray]
  declaration: |
    TByteArray = array [0..0] of Byte;
    PByteArray = ^TByteArray;
  ```

- **Set types** are generally documented together with the enumeration type they consist of, the enumeration being the main topic. A frontmatter symbol alias should be added for the set type. For example:
  ```
  aliases: [TEndStyles]
  declaration: |
    TEndStyle = (esButt, esSquare, esRound);
    TEndStyles = set of TEndStyle;
  ```

- The **interfaces** implemented by a class are documented on the class as a table, with one row per interface.
  For example, for `TGDIMemoryBackend`:
  **Implements**
  | Interface | Description |
  | --- | --- |
  | `IPaintSupport` | Interface for backends handling control repainting and invalid rect transfer to TCanvas. |
  | `IDeviceContextSupport` | Interface for backends providing native OS device context handles (HDC) and bit-blitting operations. |
- Lists of **related constants** can be documented together in separate markdown files.
  For example, all color constants are documented together in `GR32/Constants/Color Constants.md`.

---

## 6. Documentation Inheritance (Virtual Routes)

To avoid duplicating property/method documentation files across derived class hierarchies (`TCustomMap` $\rightarrow$ `TCustomBitmap32` $\rightarrow$ `TBitmap32`):

1. **Single Authoring Location**: Maintainers write member documentation **once** on the ancestor class where the member is declared (e.g. `TCustomBitmap32/Properties/Width.md`).
2. **Virtual Member Route Generation**: At build time, the Virtual Member plugin (`docs/.vitepress/virtualMembers.ts`) checks the `inheritance` list in derived class `index.md` files (e.g., `TBitmap32/index.md`).
3. **Automatic Inheritance**: If `TBitmap32/Properties/Width.md` does not exist physically on disk, a virtual route `/api/GR32/TBitmap32/Properties/Width` is generated automatically, inheriting `summary`, `parameters`, and `overloads` from `TCustomBitmap32.Width`.
4. **Inherited Sidebar Merger**: The sidebar builder (`docs/.vitepress/sidebar.ts`) traces class inheritance chains and automatically merges inherited properties and methods into derived class sidebars with clean URLs (`/api/GR32/TBitmap32/Properties/Width`).
5. **Inheritance Badge**: `ApiPage.vue` displays an `Inherited from TCustomBitmap32.Width` badge and link whenever `inheritedFrom` is present.

---

## 7. Source Signature Extraction Rules

When populating API documentation from Pascal source code in `Source/`:

1. Locate public interface declarations in the `.pas` file.
2. Group all overloads under the single member document (e.g., `TBitmap32.Draw`).
3. Preserve Pascal keywords (`const`, `var`, `out`, `overload`, `override`, `virtual`).
4. Ensure parameter names, types, and defaults match source code interface signatures accurately.

---

## 8. Documentation authorities

When authoring documentation from scratch, the following sources can be used:

1. The Single Source of Truth is the source code in `/source`.
   The code might contain comments that describe the topic. Otherwise the code can be analyzed to determine what it does.
2. Issue discussions at the Github issue tracker often contain explanations of features: https://github.com/graphics32/graphics32/issues?q=is%3Aissue
3. Google (but beware of AI feedback loops).

---

## 9. Building & Verification Commands

To verify changes and build the static site:

```bash
# Start local development server with hot reload
npm run docs:dev

# Build static production site to docs/.vitepress/dist
npm run docs:build

# Preview static production build on http://localhost:4173
npm run docs:preview
```

---

## 10. Exhaustive Unit Progress Checklist

Below is the complete, canonical list of all Pascal source units in `Source/`. AI agents and maintainers must use this checklist when populating or auditing API documentation coverage.

- [x] **GR32**
- [x] **GR32.BigEndian**: `docs/api/GR32.BigEndian/index.md`
- [ ] **GR32.Blend.Assembler** (document only at unit level)
- [ ] **GR32.Blend.Modes**
- [x] **GR32.Blend.Modes.Extra**: `docs/api/GR32.Blend.Modes.Extra/index.md` (document only at unit level)
- [x] **GR32.Blend.Modes.PhotoShop**: `docs/api/GR32.Blend.Modes.PhotoShop/index.md` (document only at unit level)
- [x] **GR32.Blend.Modes.PorterDuff**: `docs/api/GR32.Blend.Modes.PorterDuff/index.md` (document only at unit level)
- [ ] **GR32.Blend.Pascal** (document only at unit level)
- [ ] **GR32.Blend.SSE2** (document only at unit level)
- [x] **GR32.Blur**: `docs/api/GR32.Blur/index.md`
- [ ] **GR32.Blur.RecursiveGaussian**
- [x] **GR32.Blur.SelectiveGaussian**: `docs/api/GR32.Blur.SelectiveGaussian/index.md`
- [x] **GR32.CPUID**: `docs/api/GR32.CPUID/index.md`
- [ ] **GR32.Examples** (document only at unit level)
- [x] **GR32.ImageFormats**: `docs/api/GR32.ImageFormats/index.md`
- [x] **GR32.ImageFormats.BMP**: `docs/api/GR32.ImageFormats.BMP/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.Default**: `docs/api/GR32.ImageFormats.Default/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.GIF**: `docs/api/GR32.ImageFormats.GIF/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.JPG**: `docs/api/GR32.ImageFormats.JPG/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PNG**: `docs/api/GR32.ImageFormats.PNG/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PNG32**: `docs/api/GR32.ImageFormats.PNG32/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PSD**: `docs/api/GR32.ImageFormats.PSD/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PSD.Model**: `docs/api/GR32.ImageFormats.PSD.Model/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PSD.Reader**: `docs/api/GR32.ImageFormats.PSD.Reader/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PSD.Types**: `docs/api/GR32.ImageFormats.PSD.Types/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.PSD.Writer**: `docs/api/GR32.ImageFormats.PSD.Writer/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.SVG**: `docs/api/GR32.ImageFormats.SVG/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TBitmap**: `docs/api/GR32.ImageFormats.TBitmap/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TClipboard**: `docs/api/GR32.ImageFormats.TClipboard/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TGraphic**: `docs/api/GR32.ImageFormats.TGraphic/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TIcon**: `docs/api/GR32.ImageFormats.TIcon/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TMetaFile**: `docs/api/GR32.ImageFormats.TMetaFile/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TPicture**: `docs/api/GR32.ImageFormats.TPicture/index.md` (document only at unit level)
- [x] **GR32.ImageFormats.TWICImage**: `docs/api/GR32.ImageFormats.TWICImage/index.md` (document only at unit level)
- [x] **GR32.Math.Complex**
- [x] **GR32.Noise.Simplex**: `docs/api/GR32.Noise.Simplex/index.md`
- [ ] **GR32.Paint.Brush**
- [ ] **GR32.Paint.Controller**
- [ ] **GR32.Paint.Controller.API**
- [ ] **GR32.Paint.Host**
- [ ] **GR32.Paint.Host.API**
- [ ] **GR32.Paint.MouseController**
- [ ] **GR32.Paint.MouseController.API**
- [ ] **GR32.Paint.Tool**
- [ ] **GR32.Paint.Tool.API**
- [ ] **GR32.Paint.Tool.Brush**
- [ ] **GR32.Paint.Tool.Pen**
- [ ] **GR32.Paint.ToolContext**
- [ ] **GR32.Text.Cache** (document only at unit level)
- [ ] **GR32.Text.FontFace** (document only at unit level)
- [ ] **GR32.Text.Layout** (document only at unit level)
- [ ] **GR32.Text.Types**
- [ ] **GR32.Text.Unicode** (document only at unit level)
- [ ] **GR32.Text.Win** (document only at unit level)
- [X] **GR32.Transpose**
- [ ] **GR32.Types.SIMD** (document only at unit level)
- [x] **GR32_ArrowHeads**: `docs/api/GR32_ArrowHeads/index.md`
- [x] **GR32_Backends**: `docs/api/GR32_Backends/index.md`
- [x] **GR32_Backends_Generic**: `docs/api/GR32_Backends_Generic/index.md`
- [x] **GR32_Backends_LCL_Win**: `docs/api/GR32_Backends_LCL_Win/index.md`
- [x] **GR32_Backends_VCL**: `docs/api/GR32_Backends_VCL/index.md`
- [ ] **GR32_Bindings**
- [x] **GR32_Blend**: `docs/api/GR32_Blend/index.md`
- [ ] **GR32_Blurs** (document only at unit level)
- [x] **GR32_Brushes**: `docs/api/GR32_Brushes/index.md`
- [ ] **GR32_Clipboard**
- [x] **GR32_ColorGradients**: `docs/api/GR32_ColorGradients/index.md`
- [ ] **GR32_ColorPicker**
- [ ] **GR32_ColorSwatch**
- [ ] **GR32_Containers**
- [ ] **GR32_ExtImage**
- [x] **GR32_Filters**: `docs/api/GR32_Filters/index.md`
- [x] **GR32_Gamma**: `docs/api/GR32_Gamma/index.md`
- [x] **GR32_Geometry**: `docs/api/GR32_Geometry/index.md`
- [x] **GR32_Image**: `docs/api/GR32_Image/index.md`
- [x] **GR32_Layers**: `docs/api/GR32_Layers/index.md`
- [x] **GR32_LowLevel**: `docs/api/GR32_LowLevel/index.md`
- [x] **GR32_Math**: `docs/api/GR32_Math/index.md`
- [ ] **GR32_MicroTiles**
- [X] **GR32_Paths**
- [x] **GR32_OrdinalMaps**: `docs/api/GR32_OrdinalMaps/index.md`
- [ ] **GR32_Png**
- [x] **GR32_Polygons**: `docs/api/GR32_Polygons/index.md`
- [ ] **GR32_PortableNetworkGraphic** (document only at unit level)
- [ ] **GR32_RangeBars**
- [x] **GR32_Rasterizers**: `docs/api/GR32_Rasterizers/index.md`
- [ ] **GR32_RepaintOpt** (document only at unit level)
- [x] **GR32_Resamplers**: `docs/api/GR32_Resamplers/index.md`
- [x] **GR32_System**: `docs/api/GR32_System/index.md`
- [x] **GR32_Transforms**: `docs/api/GR32_Transforms/index.md`
- [x] **GR32_VPR**: `docs/api/GR32_VPR/index.md`
- [x] **GR32_VectorMaps**: `docs/api/GR32_VectorMaps/index.md`
- [x] **GR32_VectorUtils**: `docs/api/GR32_VectorUtils/index.md`
- [x] **GR32_VectorUtils.Angus**`: ``docs/api/GR32_VectorUtils.Angus/index.md` (document only at unit level)
- [x] **GR32_VectorUtils.Clipper2**`: ``docs/api/GR32_VectorUtils.Clipper2/index.md` (document only at unit level)
- [x] **GR32_VectorUtils.Reference**`: ``docs/api/GR32_VectorUtils.Reference/index.md` (document only at unit level)
- [ ] **amEasing**

The following files will not be documented. Either because they are externals (copied from other libraries), because they are internal to Graphics32, because they are obsolete, or because I can't be assed to do so:

- **Clipper**
- **Clipper.Core**
- **Clipper.Engine**
- **Clipper.Minkowski**
- **Clipper.Offset**
- **Clipper.RectClip**
- **GR32_Clipper**
- **GR32_Clipper1**
- **GR32_Clipper2**
- **GR32_Math_FPC**
- **GR32_Text_VCL_D2D**
- **GR32_Backends_LCL_Carbon**
- **GR32_Backends_LCL_CustomDrawn**
- **GR32_Backends_LCL_Gtk**
- **GR32_VPR2**

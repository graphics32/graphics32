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
| `kind` | String | Entity classification (`Class`, `Method`, `Constructor`, `Property`, `Function`, `Type`, `Constant`). |
| `scope` | String | Optional. Member visibility scope (`Public`, `Protected`, `Published`). Renders a styled scope badge in headers. |
| `summary` | String | High-level summary description. Keep short. Avoid details that are better described in the content. Often used in tables. |
| `declaration` | String | Pascal procedure/function/type signature for single-signature pages. |
| `parameters` | Array | Parameter list objects `[ { name, type, description } ]`. |
| `overloads` | Array | Array of overload objects for overloaded methods/routines. |
| `inheritedFrom` | String | Optional. Full identifier of base class member if inherited (e.g., `TCustomBitmap32.Width`). |
| `aliases` | Array / String | Optional. List of additional symbol names mapped to this page for `[[symbol]]` resolution (e.g. `aliases: [clBlack32, clWhite32]`). |

---

### Schema A: Single Signature Page
````yaml
---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Clear
kind: Method
declaration: "procedure Clear(Color: TColor32);"
summary: "Fills the entire pixel buffer with a specified TColor32 value."
parameters:
  - name: Color
    type: TColor32
    description: "32-bit ARGB color value to fill the bitmap with."
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
2. The secondary source is the old documentation: https://github.com/graphics32/graphics32.github.io/tree/master/Docs/Units (and below).
   This source is largely outdated and should not be trusted without verification against the source code.
3. Issue discussions at the Github issue tracker often contain explanations of features: https://github.com/graphics32/graphics32/issues?q=is%3Aissue
4. Google (but beware of AI feedback loops).

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

Below is the complete, canonical list of all Pascal source units in `Source/`. AI agents and maintainers must use this checklist when populating or auditing API documentation coverage:

- [x] **GR32**
  - **Classes**:
    - [x] `TPlainInterfacedPersistent` -> `docs/api/GR32/TPlainInterfacedPersistent/index.md`
      - **Properties**
        - [x] `RefCount` -> `docs/api/GR32/TPlainInterfacedPersistent/Properties/RefCount.md`
        - [x] `RefCounted` -> `docs/api/GR32/TPlainInterfacedPersistent/Properties/RefCounted.md`
    - [x] `TNotifiablePersistent` -> `docs/api/GR32/TNotifiablePersistent/index.md`
      - **Methods**
        - [x] `BeginUpdate` -> `docs/api/GR32/TNotifiablePersistent/Methods/BeginUpdate.md`
        - [x] `EndUpdate` -> `docs/api/GR32/TNotifiablePersistent/Methods/EndUpdate.md`
        - [x] `BeginLockUpdate` -> `docs/api/GR32/TNotifiablePersistent/Methods/BeginLockUpdate.md`
        - [x] `EndLockUpdate` -> `docs/api/GR32/TNotifiablePersistent/Methods/EndLockUpdate.md`
        - [x] `Changed` -> `docs/api/GR32/TNotifiablePersistent/Methods/Changed.md`
        - [x] `DoChanged` -> `docs/api/GR32/TNotifiablePersistent/Methods/DoChanged.md`
      - **Properties**
        - [x] `UpdateCount` -> `docs/api/GR32/TNotifiablePersistent/Properties/UpdateCount.md`
        - [x] `LockUpdateCount` -> `docs/api/GR32/TNotifiablePersistent/Properties/LockUpdateCount.md`
        - [x] `Modified` -> `docs/api/GR32/TNotifiablePersistent/Properties/Modified.md`
      - **Events**
        - [x] `OnChange` -> `docs/api/GR32/TNotifiablePersistent/Events/OnChange.md`
    - [x] `TThreadPersistent` -> `docs/api/GR32/TThreadPersistent/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32/TThreadPersistent/Constructors/Create.md`
      - **Methods**
        - [x] `Lock` -> `docs/api/GR32/TThreadPersistent/Methods/Lock.md`
        - [x] `Unlock` -> `docs/api/GR32/TThreadPersistent/Methods/Unlock.md`
      - **Properties**
        - [x] `LockCount` -> `docs/api/GR32/TThreadPersistent/Properties/LockCount.md`
    - [x] `TCustomMap` -> `docs/api/GR32/TCustomMap/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32/TCustomMap/Constructors/Create.md`
      - **Methods**
        - [x] `Clear` -> `docs/api/GR32/TCustomMap/Methods/Clear.md`
        - [x] `Delete` -> `docs/api/GR32/TCustomMap/Methods/Delete.md`
        - [x] `Empty` -> `docs/api/GR32/TCustomMap/Methods/Empty.md`
        - [x] `Resized` -> `docs/api/GR32/TCustomMap/Methods/Resized.md`
        - [x] `SetSize` -> `docs/api/GR32/TCustomMap/Methods/SetSize.md`
        - [x] `SetSizeFrom` -> `docs/api/GR32/TCustomMap/Methods/SetSizeFrom.md`
        - [x] `ChangeSize` -> `docs/api/GR32/TCustomMap/Methods/ChangeSize.md`
        - [x] `SetHeight` -> `docs/api/GR32/TCustomMap/Methods/SetHeight.md`
        - [x] `SetWidth` -> `docs/api/GR32/TCustomMap/Methods/SetWidth.md`
      - **Properties**
        - [x] `Height` -> `docs/api/GR32/TCustomMap/Properties/Height.md`
        - [x] `Width` -> `docs/api/GR32/TCustomMap/Properties/Width.md`
      - **Events**
        - [x] `OnResize` -> `docs/api/GR32/TCustomMap/Events/OnResize.md`
    - [x] `TCustomBitmap32` -> `docs/api/GR32/TCustomBitmap32/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32/TCustomBitmap32/Constructors/Create.md`
      - **Methods**
        - [x] `Assign` -> `docs/api/GR32/TCustomBitmap32/Methods/Assign.md`
        - [x] `BeginMeasuring` -> `docs/api/GR32/TCustomBitmap32/Methods/BeginMeasuring.md`
        - [x] `BoundsRect` -> `docs/api/GR32/TCustomBitmap32/Methods/BoundsRect.md`
        - [x] `Changed` -> `docs/api/GR32/TCustomBitmap32/Methods/Changed.md`
        - [x] `Clear` -> `docs/api/GR32/TCustomBitmap32/Methods/Clear.md`
        - [x] `CopyMapTo` -> `docs/api/GR32/TCustomBitmap32/Methods/CopyMapTo.md`
        - [x] `Delete` -> `docs/api/GR32/TCustomBitmap32/Methods/Delete.md`
        - [x] `Empty` -> `docs/api/GR32/TCustomBitmap32/Methods/Empty.md`
        - [x] `EndMeasuring` -> `docs/api/GR32/TCustomBitmap32/Methods/EndMeasuring.md`
        - [x] `GetPlatformBackendClass` -> `docs/api/GR32/TCustomBitmap32/Methods/GetPlatformBackendClass.md`
        - [x] `PropertyChanged` -> `docs/api/GR32/TCustomBitmap32/Methods/PropertyChanged.md`
        - [x] `ReleaseBackend` -> `docs/api/GR32/TCustomBitmap32/Methods/ReleaseBackend.md`
        - [x] `ResetClipRect` -> `docs/api/GR32/TCustomBitmap32/Methods/ResetClipRect.md`
        - [x] `LoadFromFile` -> `docs/api/GR32/TCustomBitmap32/Methods/LoadFromFile.md`
        - [x] `LoadFromResourceID` -> `docs/api/GR32/TCustomBitmap32/Methods/LoadFromResourceID.md`
        - [x] `LoadFromResourceName` -> `docs/api/GR32/TCustomBitmap32/Methods/LoadFromResourceName.md`
        - [x] `LoadFromStream` -> `docs/api/GR32/TCustomBitmap32/Methods/LoadFromStream.md`
        - [x] `SaveToFile` -> `docs/api/GR32/TCustomBitmap32/Methods/SaveToFile.md`
        - [x] `SaveToStream` -> `docs/api/GR32/TCustomBitmap32/Methods/SaveToStream.md`
        - [x] `Draw` -> `docs/api/GR32/TCustomBitmap32/Methods/Draw.md`
        - [x] `DrawTo` -> `docs/api/GR32/TCustomBitmap32/Methods/DrawTo.md`
        - [x] `ResetAlpha` -> `docs/api/GR32/TCustomBitmap32/Methods/ResetAlpha.md`
        - [x] `SetPixelT` -> `docs/api/GR32/TCustomBitmap32/Methods/SetPixelT.md`
        - [x] `SetPixelTS` -> `docs/api/GR32/TCustomBitmap32/Methods/SetPixelTS.md`
        - [x] `HorzLine` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLine.md`
        - [x] `HorzLineS` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLineS.md`
        - [x] `HorzLineT` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLineT.md`
        - [x] `HorzLineTS` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLineTS.md`
        - [x] `HorzLineTSP` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLineTSP.md`
        - [x] `HorzLineX` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLineX.md`
        - [x] `HorzLineXS` -> `docs/api/GR32/TCustomBitmap32/Methods/HorzLineXS.md`
        - [x] `VertLine` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLine.md`
        - [x] `VertLineS` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLineS.md`
        - [x] `VertLineT` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLineT.md`
        - [x] `VertLineTS` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLineTS.md`
        - [x] `VertLineTSP` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLineTSP.md`
        - [x] `VertLineX` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLineX.md`
        - [x] `VertLineXS` -> `docs/api/GR32/TCustomBitmap32/Methods/VertLineXS.md`
        - [x] `Line` -> `docs/api/GR32/TCustomBitmap32/Methods/Line.md`
        - [x] `LineS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineS.md`
        - [x] `LineT` -> `docs/api/GR32/TCustomBitmap32/Methods/LineT.md`
        - [x] `LineTS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineTS.md`
        - [x] `LineA` -> `docs/api/GR32/TCustomBitmap32/Methods/LineA.md`
        - [x] `LineAS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineAS.md`
        - [x] `LineX` -> `docs/api/GR32/TCustomBitmap32/Methods/LineX.md`
        - [x] `LineF` -> `docs/api/GR32/TCustomBitmap32/Methods/LineF.md`
        - [x] `LineXS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineXS.md`
        - [x] `LineFS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineFS.md`
        - [x] `LineXP` -> `docs/api/GR32/TCustomBitmap32/Methods/LineXP.md`
        - [x] `LineFP` -> `docs/api/GR32/TCustomBitmap32/Methods/LineFP.md`
        - [x] `LineXSP` -> `docs/api/GR32/TCustomBitmap32/Methods/LineXSP.md`
        - [x] `LineFSP` -> `docs/api/GR32/TCustomBitmap32/Methods/LineFSP.md`
        - [x] `MoveTo` -> `docs/api/GR32/TCustomBitmap32/Methods/MoveTo.md`
        - [x] `MoveToX` -> `docs/api/GR32/TCustomBitmap32/Methods/MoveToX.md`
        - [x] `MoveToF` -> `docs/api/GR32/TCustomBitmap32/Methods/MoveToF.md`
        - [x] `LineToS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToS.md`
        - [x] `LineToTS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToTS.md`
        - [x] `LineToAS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToAS.md`
        - [x] `LineToXS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToXS.md`
        - [x] `LineToFS` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToFS.md`
        - [x] `LineToXSP` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToXSP.md`
        - [x] `LineToFSP` -> `docs/api/GR32/TCustomBitmap32/Methods/LineToFSP.md`
        - [x] `FillRect` -> `docs/api/GR32/TCustomBitmap32/Methods/FillRect.md`
        - [x] `FillRectS` -> `docs/api/GR32/TCustomBitmap32/Methods/FillRectS.md`
        - [x] `FillRectT` -> `docs/api/GR32/TCustomBitmap32/Methods/FillRectT.md`
        - [x] `FillRectTS` -> `docs/api/GR32/TCustomBitmap32/Methods/FillRectTS.md`
        - [x] `FrameRectS` -> `docs/api/GR32/TCustomBitmap32/Methods/FrameRectS.md`
        - [x] `FrameRectTS` -> `docs/api/GR32/TCustomBitmap32/Methods/FrameRectTS.md`
        - [x] `FrameRectTSP` -> `docs/api/GR32/TCustomBitmap32/Methods/FrameRectTSP.md`
        - [x] `RaiseRectTS` -> `docs/api/GR32/TCustomBitmap32/Methods/RaiseRectTS.md`
        - [x] `Roll` -> `docs/api/GR32/TCustomBitmap32/Methods/Roll.md`
        - [x] `FlipHorz` -> `docs/api/GR32/TCustomBitmap32/Methods/FlipHorz.md`
        - [x] `FlipVert` -> `docs/api/GR32/TCustomBitmap32/Methods/FlipVert.md`
        - [x] `Rotate90` -> `docs/api/GR32/TCustomBitmap32/Methods/Rotate90.md`
        - [x] `Rotate180` -> `docs/api/GR32/TCustomBitmap32/Methods/Rotate180.md`
        - [x] `Rotate270` -> `docs/api/GR32/TCustomBitmap32/Methods/Rotate270.md`
        - [x] `SetStipple` -> `docs/api/GR32/TCustomBitmap32/Methods/SetStipple.md`
        - [x] `AdvanceStippleCounter` -> `docs/api/GR32/TCustomBitmap32/Methods/AdvanceStippleCounter.md`
        - [x] `GetStippleColor` -> `docs/api/GR32/TCustomBitmap32/Methods/GetStippleColor.md`
      - **Properties**
        - [x] `Pixel` -> `docs/api/GR32/TCustomBitmap32/Properties/Pixel.md`
        - [x] `PixelS` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelS.md`
        - [x] `PixelW` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelW.md`
        - [x] `PixelX` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelX.md`
        - [x] `PixelXS` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelXS.md`
        - [x] `PixelXW` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelXW.md`
        - [x] `PixelF` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelF.md`
        - [x] `PixelFS` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelFS.md`
        - [x] `PixelFW` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelFW.md`
        - [x] `PixelFR` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelFR.md`
        - [x] `PixelXR` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelXR.md`
        - [x] `PenColor` -> `docs/api/GR32/TCustomBitmap32/Properties/PenColor.md`
        - [x] `PenPos` -> `docs/api/GR32/TCustomBitmap32/Properties/PenPos.md`
        - [x] `PenPosF` -> `docs/api/GR32/TCustomBitmap32/Properties/PenPosF.md`
        - [x] `StippleCounter` -> `docs/api/GR32/TCustomBitmap32/Properties/StippleCounter.md`
        - [x] `StippleStep` -> `docs/api/GR32/TCustomBitmap32/Properties/StippleStep.md`
        - [x] `Backend` -> `docs/api/GR32/TCustomBitmap32/Properties/Backend.md`
        - [x] `Bits` -> `docs/api/GR32/TCustomBitmap32/Properties/Bits.md`
        - [x] `ClipRect` -> `docs/api/GR32/TCustomBitmap32/Properties/ClipRect.md`
        - [x] `Clipping` -> `docs/api/GR32/TCustomBitmap32/Properties/Clipping.md`
        - [x] `MeasuringMode` -> `docs/api/GR32/TCustomBitmap32/Properties/MeasuringMode.md`
        - [x] `PixelPtr` -> `docs/api/GR32/TCustomBitmap32/Properties/PixelPtr.md`
        - [x] `ScanLine` -> `docs/api/GR32/TCustomBitmap32/Properties/ScanLine.md`
        - [x] `DrawMode` -> `docs/api/GR32/TCustomBitmap32/Properties/DrawMode.md`
        - [x] `CombineMode` -> `docs/api/GR32/TCustomBitmap32/Properties/CombineMode.md`
        - [x] `WrapMode` -> `docs/api/GR32/TCustomBitmap32/Properties/WrapMode.md`
        - [x] `MasterAlpha` -> `docs/api/GR32/TCustomBitmap32/Properties/MasterAlpha.md`
        - [x] `OuterColor` -> `docs/api/GR32/TCustomBitmap32/Properties/OuterColor.md`
        - [x] `ResamplerClassName` -> `docs/api/GR32/TCustomBitmap32/Properties/ResamplerClassName.md`
        - [x] `Resampler` -> `docs/api/GR32/TCustomBitmap32/Properties/Resampler.md`
      - **Events**
        - [x] `OnPixelCombine` -> `docs/api/GR32/TCustomBitmap32/Events/OnPixelCombine.md`
        - [x] `OnAreaChanged` -> `docs/api/GR32/TCustomBitmap32/Events/OnAreaChanged.md`
    - [x] `TBitmap32` -> `docs/api/GR32/TBitmap32/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32/TBitmap32/Constructors/Create.md`
      - **Methods**
        - [x] `Draw` -> `docs/api/GR32/TBitmap32/Methods/Draw.md`
        - [x] `DrawTo` -> `docs/api/GR32/TBitmap32/Methods/DrawTo.md`
        - [x] `GetPlatformBackendClass` -> `docs/api/GR32/TBitmap32/Methods/GetPlatformBackendClass.md`
        - [x] `TileTo` -> `docs/api/GR32/TBitmap32/Methods/TileTo.md`
      - **Properties**
        - [x] `BitmapInfo` -> `docs/api/GR32/TBitmap32/Properties/BitmapInfo.md`
        - [x] `Canvas` -> `docs/api/GR32/TBitmap32/Properties/Canvas.md`
        - [x] `Font` -> `docs/api/GR32/TBitmap32/Properties/Font.md`
        - [x] `Handle` -> `docs/api/GR32/TBitmap32/Properties/Handle.md`
        - [x] `HDC` -> `docs/api/GR32/TBitmap32/Properties/HDC.md`
      - **Events**
        - [x] `OnHandleChanged` -> `docs/api/GR32/TBitmap32/Events/OnHandleChanged.md`
    - [x] `TCustomSampler` -> `docs/api/GR32/TCustomSampler/index.md`
      - **Methods**
        - [x] `GetSampleInt` -> `docs/api/GR32/TCustomSampler/Methods/GetSampleInt.md`
        - [x] `GetSampleFixed` -> `docs/api/GR32/TCustomSampler/Methods/GetSampleFixed.md`
        - [x] `GetSampleFloat` -> `docs/api/GR32/TCustomSampler/Methods/GetSampleFloat.md`
        - [x] `PrepareSampling` -> `docs/api/GR32/TCustomSampler/Methods/PrepareSampling.md`
        - [x] `FinalizeSampling` -> `docs/api/GR32/TCustomSampler/Methods/FinalizeSampling.md`
        - [x] `HasBounds` -> `docs/api/GR32/TCustomSampler/Methods/HasBounds.md`
        - [x] `GetSampleBounds` -> `docs/api/GR32/TCustomSampler/Methods/GetSampleBounds.md`
    - [x] `TCustomResampler` -> `docs/api/GR32/TCustomResampler/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32/TCustomResampler/Constructors/Create.md`
      - **Methods**
        - [x] `Resample` -> `docs/api/GR32/TCustomResampler/Methods/Resample.md`
      - **Properties**
        - [x] `Bitmap` -> `docs/api/GR32/TCustomResampler/Properties/Bitmap.md`
        - [x] `Width` -> `docs/api/GR32/TCustomResampler/Properties/Width.md`
        - [x] `PixelAccessMode` -> `docs/api/GR32/TCustomResampler/Properties/PixelAccessMode.md`
    - [x] `TCustomBackend` -> `docs/api/GR32/TCustomBackend/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32/TCustomBackend/Constructors/Create.md`
      - **Methods**
        - [x] `Assign` -> `docs/api/GR32/TCustomBackend/Methods/Assign.md`
        - [x] `ChangeSize` -> `docs/api/GR32/TCustomBackend/Methods/ChangeSize.md`
        - [x] `Changing` -> `docs/api/GR32/TCustomBackend/Methods/Changing.md`
        - [x] `Clear` -> `docs/api/GR32/TCustomBackend/Methods/Clear.md`
        - [x] `Empty` -> `docs/api/GR32/TCustomBackend/Methods/Empty.md`
        - [x] `FinalizeSurface` -> `docs/api/GR32/TCustomBackend/Methods/FinalizeSurface.md`
        - [x] `InitializeSurface` -> `docs/api/GR32/TCustomBackend/Methods/InitializeSurface.md`
      - **Properties**
        - [x] `Bits` -> `docs/api/GR32/TCustomBackend/Properties/Bits.md`
      - **Events**
        - [x] `OnChanging` -> `docs/api/GR32/TCustomBackend/Events/OnChanging.md`
    - [x] `TCustomBackendClass`
    - [x] `TCustomBitmap32Class`
    - [x] `TCustomResamplerClass`
  - **Functions**:
    - [x] `Color32`
    - [x] `AlphaComponent`
    - [x] `RedComponent`
    - [x] `GreenComponent`
    - [x] `BlueComponent`
    - [x] `SetAlpha`
    - [x] `Intensity`
    - [x] `RGBtoHSV` / `HSVtoRGB`
  - **Records**:
    - [x] `TColor32Entry`
    - [x] `TFixedPoint`
    - [x] `TFixedRec`
    - [x] `TFixedRect`
    - [x] `TFloatPoint`
    - [x] `TFloatRect`
  - **Interfaces**:
    - *(None)*
  - **Constants**:
    - [x] `clBlack32`, `clWhite32`, `clRed32`, `clGreen32`, `clBlue32`, etc.
  - **Other Types**:
    - [x] `TColor32`
    - [x] `TArrayOfColor32`
    - [x] `PColor32`
    - [x] `TFixed`
- [ ] **GR32.BigEndian**
- [ ] **GR32.Blend.Assembler** (document only at unit level)
- [ ] **GR32.Blend.Modes**
- [ ] **GR32.Blend.Modes.Extra**
- [ ] **GR32.Blend.Modes.PhotoShop**
- [ ] **GR32.Blend.Modes.PorterDuff**
- [ ] **GR32.Blend.Pascal** (document only at unit level)
- [ ] **GR32.Blend.SSE2** (document only at unit level)
- [ ] **GR32.Blur**
- [ ] **GR32.Blur.RecursiveGaussian**
- [ ] **GR32.Blur.SelectiveGaussian**
- [ ] **GR32.CPUID**
- [ ] **GR32.Examples** (document only at unit level)
- [ ] **GR32.ImageFormats**
- [ ] **GR32.ImageFormats.BMP**
- [ ] **GR32.ImageFormats.Default** (document only at unit level)
- [ ] **GR32.ImageFormats.GIF**
- [ ] **GR32.ImageFormats.JPG**
- [ ] **GR32.ImageFormats.PNG**
- [ ] **GR32.ImageFormats.PNG32**
- [ ] **GR32.ImageFormats.PSD**
- [ ] **GR32.ImageFormats.PSD.Model** (document only at unit level)
- [ ] **GR32.ImageFormats.PSD.Reader** (document only at unit level)
- [ ] **GR32.ImageFormats.PSD.Types** (document only at unit level)
- [ ] **GR32.ImageFormats.PSD.Writer** (document only at unit level)
- [ ] **GR32.ImageFormats.SVG**
- [ ] **GR32.ImageFormats.TBitmap**
- [ ] **GR32.ImageFormats.TClipboard**
- [ ] **GR32.ImageFormats.TGraphic**
- [ ] **GR32.ImageFormats.TIcon**
- [ ] **GR32.ImageFormats.TMetaFile**
- [ ] **GR32.ImageFormats.TPicture**
- [ ] **GR32.ImageFormats.TWICImage**
- [ ] **GR32.Math.Complex**
- [ ] **GR32.Noise.Simplex**
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
- [ ] **GR32.Transpose**
- [ ] **GR32.Types.SIMD** (document only at unit level)
- [ ] **GR32_ArrowHeads**
- [x] **GR32_Backends** -> `docs/api/GR32_Backends/index.md`
  - **Classes**
    - [x] `EBackend` -> `docs/api/GR32_Backends/Classes/EBackend/index.md`
  - **Interfaces**
    - [x] `ITextSupport` -> `docs/api/GR32_Backends/Interfaces/ITextSupport.md`
    - [x] `IFontSupport` -> `docs/api/GR32_Backends/Interfaces/IFontSupport.md`
    - [x] `ITextToPathSupport` -> `docs/api/GR32_Backends/Interfaces/ITextToPathSupport.md`
    - [x] `ITextToPathSupport2` -> `docs/api/GR32_Backends/Interfaces/ITextToPathSupport2.md`
    - [x] `ICanvasSupport` -> `docs/api/GR32_Backends/Interfaces/ICanvasSupport.md`
    - [x] `IInteroperabilitySupport` -> `docs/api/GR32_Backends/Interfaces/IInteroperabilitySupport.md`
    - [x] `IDeviceContextSupport` -> `docs/api/GR32_Backends/Interfaces/IDeviceContextSupport.md`
    - [x] `IBitmapContextSupport` -> `docs/api/GR32_Backends/Interfaces/IBitmapContextSupport.md`
    - [x] `IPaintSupport` -> `docs/api/GR32_Backends/Interfaces/IPaintSupport.md`
    - [x] `IUpdateRectSupport` -> `docs/api/GR32_Backends/Interfaces/IUpdateRectSupport.md`
    - [x] `IFontHintingSupport` -> `docs/api/GR32_Backends/Interfaces/IFontHintingSupport.md`
  - **Types**
    - [x] `TTextHinting` -> `docs/api/GR32_Backends/Types/TTextHinting.md`
    - [x] `TRequireOperatorMode` -> `docs/api/GR32_Backends/Types/TRequireOperatorMode.md`
  - **Routines**
    - [x] `RequireBackendSupport` -> `docs/api/GR32_Backends/Routines/RequireBackendSupport.md`
    - [x] `RestoreBackend` -> `docs/api/GR32_Backends/Routines/RestoreBackend.md`
- [x] **GR32_Backends_Generic** -> `docs/api/GR32_Backends_Generic/index.md`
  - **Classes**
    - [x] `TMemoryBackend` -> `docs/api/GR32_Backends_Generic/Classes/TMemoryBackend/index.md`
    - [x] `TMMFBackend` -> `docs/api/GR32_Backends_Generic/Classes/TMMFBackend/index.md`
- [ ] **GR32_Backends_LCL_Carbon** (document only at unit level)
- [ ] **GR32_Backends_LCL_CustomDrawn** (document only at unit level)
- [ ] **GR32_Backends_LCL_Gtk** (document only at unit level)
- [x] **GR32_Backends_LCL_Win** -> `docs/api/GR32_Backends_LCL_Win/index.md`
  - **Classes**
    - [x] `TLCLBackend` -> `docs/api/GR32_Backends_LCL_Win/Classes/TLCLBackend/index.md`
    - [x] `TLCLMMFBackend` -> `docs/api/GR32_Backends_LCL_Win/Classes/TLCLMMFBackend/index.md`
    - [x] `TLCLMemoryBackend` -> `docs/api/GR32_Backends_LCL_Win/Classes/TLCLMemoryBackend/index.md`
- [x] **GR32_Backends_VCL** -> `docs/api/GR32_Backends_VCL/index.md`
  - **Classes**
    - [x] `TGDIBackend` -> `docs/api/GR32_Backends_VCL/Classes/TGDIBackend/index.md`
    - [x] `TGDIMMFBackend` -> `docs/api/GR32_Backends_VCL/Classes/TGDIMMFBackend/index.md`
    - [x] `TGDIMemoryBackend` -> `docs/api/GR32_Backends_VCL/Classes/TGDIMemoryBackend/index.md`
- [ ] **GR32_Bindings**
- [ ] **GR32_Blend**
- [ ] **GR32_Blurs** (document only at unit level)
- [ ] **GR32_Brushes**
- [ ] **GR32_Clipboard**
- [ ] **GR32_Clipper**
- [ ] **GR32_Clipper1** (document only at unit level)
- [ ] **GR32_Clipper2** (document only at unit level)
- [ ] **GR32_ColorGradients**
- [ ] **GR32_ColorPicker**
- [ ] **GR32_ColorSwatch**
- [ ] **GR32_Containers**
- [ ] **GR32_ExtImage**
- [x] **GR32_Filters** -> `docs/api/GR32_Filters/index.md`
  - **Types**
    - [x] `TLUT8` -> `docs/api/GR32_Filters/Types/TLUT8.md`
    - [x] `TLogicalOperator` -> `docs/api/GR32_Filters/Types/TLogicalOperator.md`
  - **Routines**
    - [x] `AlphaToGrayscale` -> `docs/api/GR32_Filters/Routines/AlphaToGrayscale.md`
    - [x] `ApplyBitmask` -> `docs/api/GR32_Filters/Routines/ApplyBitmask.md`
    - [x] `ApplyLUT` -> `docs/api/GR32_Filters/Routines/ApplyLUT.md`
    - [x] `CheckParams` -> `docs/api/GR32_Filters/Routines/CheckParams.md`
    - [x] `ChromaKey` -> `docs/api/GR32_Filters/Routines/ChromaKey.md`
    - [x] `ColorToGrayscale` -> `docs/api/GR32_Filters/Routines/ColorToGrayscale.md`
    - [x] `CopyComponents` -> `docs/api/GR32_Filters/Routines/CopyComponents.md`
    - [x] `CreateBitmask` -> `docs/api/GR32_Filters/Routines/CreateBitmask.md`
    - [x] `IntensityToAlpha` -> `docs/api/GR32_Filters/Routines/IntensityToAlpha.md`
    - [x] `Invert` -> `docs/api/GR32_Filters/Routines/Invert.md`
    - [x] `InvertRGB` -> `docs/api/GR32_Filters/Routines/InvertRGB.md`
- [ ] **GR32_Gamma**
- [ ] **GR32_Geometry**
- [ ] **GR32_Image**
- [ ] **GR32_Layers**
- [ ] **GR32_LowLevel**
- [ ] **GR32_Math**
- [ ] **GR32_Math_FPC** (document only at unit level)
- [ ] **GR32_MicroTiles**
- [ ] **GR32_OrdinalMaps**
- [ ] **GR32_Paths**
- [ ] **GR32_Png**
- [x] **GR32_Polygons** -> `docs/api/GR32_Polygons/index.md`
  - **Classes**
    - [x] `TCustomPolygonRenderer` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonRenderer/index.md`
      - **Methods**
        - [x] `PolyPolygonFS` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonRenderer/Methods/PolyPolygonFS.md`
        - [x] `PolygonFS` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonRenderer/Methods/PolygonFS.md`
    - [x] `TPolygonRenderer32` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Constructors/Create.md`
      - **Methods**
        - [x] `PolyPolygonFS` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Methods/PolyPolygonFS.md`
        - [x] `PolygonFS` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Methods/PolygonFS.md`
      - **Properties**
        - [x] `Bitmap` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Properties/Bitmap.md`
        - [x] `Color` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Properties/Color.md`
        - [x] `FillMode` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Properties/FillMode.md`
        - [x] `Filler` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32/Properties/Filler.md`
    - [x] `TPolygonRenderer32VPR` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32VPR/index.md`
      - **Methods**
        - [x] `PolyPolygonFS` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32VPR/Methods/PolyPolygonFS.md`
    - [x] `TPolygonRenderer32LCD` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32LCD/index.md`
      - **Methods**
        - [x] `PolyPolygonFS` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32LCD/Methods/PolyPolygonFS.md`
    - [x] `TPolygonRenderer32LCD2` -> `docs/api/GR32_Polygons/Classes/TPolygonRenderer32LCD2/index.md`
    - [x] `TCustomPolygonFiller` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonFiller/index.md`
      - **Methods**
        - [x] `BeginRendering` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonFiller/Methods/BeginRendering.md`
        - [x] `EndRendering` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonFiller/Methods/EndRendering.md`
      - **Properties**
        - [x] `FillLine` -> `docs/api/GR32_Polygons/Classes/TCustomPolygonFiller/Properties/FillLine.md`
    - [x] `TCallbackPolygonFiller` -> `docs/api/GR32_Polygons/Classes/TCallbackPolygonFiller/index.md`
      - **Methods**
        - [x] `BeginRendering` -> `docs/api/GR32_Polygons/Classes/TCallbackPolygonFiller/Methods/BeginRendering.md`
      - **Properties**
        - [x] `FillLineEvent` -> `docs/api/GR32_Polygons/Classes/TCallbackPolygonFiller/Properties/FillLineEvent.md`
    - [x] `TInvertPolygonFiller` -> `docs/api/GR32_Polygons/Classes/TInvertPolygonFiller/index.md`
    - [x] `TClearPolygonFiller` -> `docs/api/GR32_Polygons/Classes/TClearPolygonFiller/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32_Polygons/Classes/TClearPolygonFiller/Constructors/Create.md`
      - **Properties**
        - [x] `Color` -> `docs/api/GR32_Polygons/Classes/TClearPolygonFiller/Properties/Color.md`
    - [x] `TBitmapPolygonFiller` -> `docs/api/GR32_Polygons/Classes/TBitmapPolygonFiller/index.md`
      - **Methods**
        - [x] `BeginRendering` -> `docs/api/GR32_Polygons/Classes/TBitmapPolygonFiller/Methods/BeginRendering.md`
      - **Properties**
        - [x] `Pattern` -> `docs/api/GR32_Polygons/Classes/TBitmapPolygonFiller/Properties/Pattern.md`
        - [x] `OffsetX` -> `docs/api/GR32_Polygons/Classes/TBitmapPolygonFiller/Properties/OffsetX.md`
        - [x] `OffsetY` -> `docs/api/GR32_Polygons/Classes/TBitmapPolygonFiller/Properties/OffsetY.md`
    - [x] `TSamplerFiller` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/index.md`
      - **Constructors**
        - [x] `Create` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/Constructors/Create.md`
      - **Methods**
        - [x] `BeginRendering` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/Methods/BeginRendering.md`
        - [x] `EndRendering` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/Methods/EndRendering.md`
      - **Properties**
        - [x] `Sampler` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/Properties/Sampler.md`
        - [x] `OwnsSampler` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/Properties/OwnsSampler.md`
        - [x] `BlendOpaque` -> `docs/api/GR32_Polygons/Classes/TSamplerFiller/Properties/BlendOpaque.md`
  - **Interfaces**
    - [x] `IPolygonRendererBatching` -> `docs/api/GR32_Polygons/Interfaces/IPolygonRendererBatching.md`
  - **Types**
    - [x] `TJoinStyle` -> `docs/api/GR32_Polygons/Types/TJoinStyle.md`
    - [x] `TJoinStyles` -> `docs/api/GR32_Polygons/Types/TJoinStyles.md`
    - [x] `TEndStyle` -> `docs/api/GR32_Polygons/Types/TEndStyle.md`
    - [x] `TEndStyles` -> `docs/api/GR32_Polygons/Types/TEndStyles.md`
    - [x] `TPolyFillMode` -> `docs/api/GR32_Polygons/Types/TPolyFillMode.md`
    - [x] `TFillProc` -> `docs/api/GR32_Polygons/Types/TFillProc.md`
    - [x] `TFillLineEvent` -> `docs/api/GR32_Polygons/Types/TFillLineEvent.md`
    - [x] `TCustomPolygonRendererList` -> `docs/api/GR32_Polygons/Types/TCustomPolygonRendererList.md`
    - [x] `TPolygonRendererList` -> `docs/api/GR32_Polygons/Types/TPolygonRendererList.md`
  - **Routines**
    - [x] `PolygonFS` -> `docs/api/GR32_Polygons/Routines/PolygonFS.md`
    - [x] `PolyPolygonFS` -> `docs/api/GR32_Polygons/Routines/PolyPolygonFS.md`
    - [x] `PolygonFS_LCD` -> `docs/api/GR32_Polygons/Routines/PolygonFS_LCD.md`
    - [x] `PolyPolygonFS_LCD` -> `docs/api/GR32_Polygons/Routines/PolyPolygonFS_LCD.md`
    - [x] `PolygonFS_LCD2` -> `docs/api/GR32_Polygons/Routines/PolygonFS_LCD2.md`
    - [x] `PolyPolygonFS_LCD2` -> `docs/api/GR32_Polygons/Routines/PolyPolygonFS_LCD2.md`
    - [x] `PolygonXS` -> `docs/api/GR32_Polygons/Routines/PolygonXS.md`
    - [x] `PolyPolygonXS` -> `docs/api/GR32_Polygons/Routines/PolyPolygonXS.md`
    - [x] `PolygonXS_LCD` -> `docs/api/GR32_Polygons/Routines/PolygonXS_LCD.md`
    - [x] `PolyPolygonXS_LCD` -> `docs/api/GR32_Polygons/Routines/PolyPolygonXS_LCD.md`
    - [x] `PolygonXS_LCD2` -> `docs/api/GR32_Polygons/Routines/PolygonXS_LCD2.md`
    - [x] `PolyPolygonXS_LCD2` -> `docs/api/GR32_Polygons/Routines/PolyPolygonXS_LCD2.md`
    - [x] `PolylineFS` -> `docs/api/GR32_Polygons/Routines/PolylineFS.md`
    - [x] `PolyPolylineFS` -> `docs/api/GR32_Polygons/Routines/PolyPolylineFS.md`
    - [x] `PolylineXS` -> `docs/api/GR32_Polygons/Routines/PolylineXS.md`
    - [x] `PolyPolylineXS` -> `docs/api/GR32_Polygons/Routines/PolyPolylineXS.md`
    - [x] `DashLineFS` -> `docs/api/GR32_Polygons/Routines/DashLineFS.md`
    - [x] `DashLineXS` -> `docs/api/GR32_Polygons/Routines/DashLineXS.md`
    - [x] `FillBitmap` -> `docs/api/GR32_Polygons/Routines/FillBitmap.md`
    - [x] `RegisterPolygonRenderer` -> `docs/api/GR32_Polygons/Routines/RegisterPolygonRenderer.md`
    - [x] `UnregisterPolygonRenderer` -> `docs/api/GR32_Polygons/Routines/UnregisterPolygonRenderer.md`
    - [x] `PolygonsRegistry` -> `docs/api/GR32_Polygons/Routines/PolygonsRegistry.md`
  - **Variables**
    - [x] `CustomPolygonRendererList` -> `docs/api/GR32_Polygons/Variables/CustomPolygonRendererList.md`
    - [x] `PolygonRendererList` -> `docs/api/GR32_Polygons/Variables/PolygonRendererList.md`
    - [x] `DefaultPolygonRendererClass` -> `docs/api/GR32_Polygons/Variables/DefaultPolygonRendererClass.md`
    - [x] `CoverageBuilderVariables` -> `docs/api/GR32_Polygons/Variables/CoverageBuilderVariables.md`
- [ ] **GR32_Polygons.AggLite** (document only at unit level)
- [ ] **GR32_Polygons.Direct2D** (document only at unit level)
- [ ] **GR32_Polygons.GDI** (document only at unit level)
- [ ] **GR32_Polygons.GDIPlus** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic**
- [ ] **GR32_PortableNetworkGraphic.Chunks** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.IDAT** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.PLTE** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.Unknown** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.bKGD** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.cHRM** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.gAMA** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.hIST** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.iCCP** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.iTXt** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.oFFs** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.pCAL** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.pHYs** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.sBIT** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.sCAL** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.sPLT** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.sRGB** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.tEXt** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.tIME** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.tRNS** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Chunks.zTXt** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Encoding** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Transcoding** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.Types** (document only at unit level)
- [ ] **GR32_PortableNetworkGraphic.ZLib** (document only at unit level)
- [ ] **GR32_RangeBars**
- [ ] **GR32_Rasterizers**
- [ ] **GR32_RepaintOpt** (document only at unit level)
- [ ] **GR32_Resamplers**
- [ ] **GR32_System**
- [ ] **GR32_Text_VCL_D2D** (document only at unit level)
- [ ] **GR32_Transforms**
- [ ] **GR32_VPR**
- [ ] **GR32_VPR2** (document only at unit level)
- [ ] **GR32_VectorMaps**
- [ ] **GR32_VectorUtils**
- [ ] **GR32_VectorUtils.Angus** (document only at unit level)
- [ ] **GR32_VectorUtils.Clipper2** (document only at unit level)
- [ ] **GR32_VectorUtils.Reference** (document only at unit level)
- [ ] **amEasing**

The following files will not be documented. Either because they are externals (copied from other libraries) or because they are internal to Graphics32:

- **Clipper**
- **Clipper.Core**
- **Clipper.Engine**
- **Clipper.Minkowski**
- **Clipper.Offset**
- **Clipper.RectClip**

---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: TCustomGraphics32ComponentBlender
kind: Class
abstract: true
declaration: |
  TCustomGraphics32ComponentBlender = class abstract(TCustomGraphics32Blender)
inheritance:
  - TObject
  - TCustomGraphics32Blender
  - TCustomGraphics32ComponentBlender
summary: "Abstract base class for component-level color blenders."
---

## Description

`TCustomGraphics32ComponentBlender` extends [[TCustomGraphics32Blender]] to process colors by separating RGB color channels from alpha channels before passing them to the protected `BlendComponents` method.

::: info
Derived classes must implement the virtual abstract [[BlendComponents]] method.
:::

[members]

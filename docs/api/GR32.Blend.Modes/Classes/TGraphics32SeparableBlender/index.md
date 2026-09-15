---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: TGraphics32SeparableBlender
kind: Class
abstract: true
declaration: |
  TGraphics32SeparableBlender = class abstract(TGraphics32ComponentBlender)
inheritance:
  - TObject
  - TCustomGraphics32Blender
  - TCustomGraphics32ComponentBlender
  - TGraphics32ComponentBlender
  - TGraphics32SeparableBlender
summary: "Abstract base class for separable blenders applying the same blend function independently across RGB channels."
---

## Description

`TGraphics32SeparableBlender` simplifies custom blender creation for separable blend modes where the same channel function (`BlendComponent`) is executed independently on the red, green, and blue color channels.

::: info
Derived classes must implement the virtual abstract [[BlendComponent]] method.
:::

[members]

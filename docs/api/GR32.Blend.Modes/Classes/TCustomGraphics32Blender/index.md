---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: TCustomGraphics32Blender
kind: Class
abstract: true
aliases: [TGraphics32BlenderClass]
declaration: |
  TCustomGraphics32Blender = class abstract

  TGraphics32BlenderClass = class of TCustomGraphics32Blender;
inheritance:
  - TObject
  - TCustomGraphics32Blender
summary: "Abstract base class for custom pixel blenders in Graphics32."
---

## Description

`TCustomGraphics32Blender` defines the fundamental contract and base interface for object-oriented pixel blenders in Graphics32. It provides virtual and abstract methods for combining foreground and background colors (`Blend`, `BlendEx`), retrieving pixel combiner function delegates (`GetPixelCombiner`), and reporting unique identifiers (`ID`) and display names (`Name`).

`TGraphics32BlenderClass` is the metaclass type for `TCustomGraphics32Blender`.

::: info
Derived classes must implement the virtual abstract methods:
- [[GetName]]
- [[Blend]]
:::

[members]

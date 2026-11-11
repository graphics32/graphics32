---
layout: doc
docType: api
unit: GR32_Layers
entity: TCustomIndirectBitmapLayer
kind: Class
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
  - TPositionedLayer
  - TCustomIndirectBitmapLayer
summary: "Base class for layers referencing a bitmap instance."
---

## Description

`TCustomIndirectBitmapLayer` displays a bitmap (`TCustomBitmap32`) within a positioned layer. The layer, by default, references an external bitmap without taking ownership, supporting pixel alpha hit testing (`AlphaHit`) and image cropping (`Cropped`).

::: info
Derived classes can alter the bitmap ownership rules by overriding the protected `OwnsBitmap` method.

For example, the derived classes [[TCustomBitmapLayer]] and [[TBitmapLayer]] owns their internal bitmaps.
:::

[members]

---
layout: doc
docType: api
unit: GR32_Layers
entity: TCustomBitmapLayer
kind: Class
abstract: true
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
  - TPositionedLayer
  - TCustomIndirectBitmapLayer
  - TCustomBitmapLayer
summary: "Abstract base class for layers that construct and own their internal bitmap instance."
---

## Description

`TCustomBitmapLayer` is an abstract base class for bitmap layers that own their internal bitmap instance. It automatically instantiates its bitmap on creation via `CreateBitmap` and destroys it when the layer is destroyed.

[members]

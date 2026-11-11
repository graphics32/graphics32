---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.Index
kind: Property
scope: Public
declaration: "property Index: Integer read ... write ...;"
summary: "Zero-based Z-order position of the layer within its LayerCollection."
seealso:
  - "[[SendToBack]]"
  - "[[BringToFront]]"
---

## Description

`Index` determines the Z-ordering position of the layer. Lower index values are rendered beneath higher index values. Higher index values receives mouse events before lower index values.

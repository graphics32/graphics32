---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.Visible
kind: Property
scope: Public
declaration: "property Visible: Boolean read ... write ...;"
summary: "Controls whether the layer is visible and rendered during paint cycles."
---

## Description

`Visible` is a convenience property that reads and sets the [[LOB_VISIBLE]] bit in [[LayerOptions]].

`True` means the bit is set, and `False` means the bit isn't set.
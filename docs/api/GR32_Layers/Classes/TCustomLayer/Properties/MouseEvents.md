---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.MouseEvents
kind: Property
scope: Public
declaration: "property MouseEvents: Boolean read ... write ...;"
summary: "Controls whether the layer responds to mouse hit testing and mouse messages."
---

## Description

`MouseEvents` is a convenience property that reads and sets the [[LOB_MOUSE_EVENTS]] bit in [[LayerOptions]].

`True` means the bit is set, and `False` means the bit isn't set.
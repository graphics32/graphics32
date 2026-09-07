---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.MouseEvents
kind: Property
scope: Public
declaration: "property MouseEvents: Boolean read FMouseEvents write SetMouseEvents;"
summary: "Controls whether mouse interaction is globally enabled for layers in the collection."
---

## Description

When `MouseEvents` is `True`, mouse actions are dispatched to layers under the cursor. Setting `MouseEvents` to `False` disables layer mouse handling and releases active mouse capture.

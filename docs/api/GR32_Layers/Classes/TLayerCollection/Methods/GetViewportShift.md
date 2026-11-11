---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.GetViewportShift
kind: Method
scope: Public
declaration: "procedure GetViewportShift(out ShiftX, ShiftY: TFloat); virtual;"
summary: "Retrieves the current horizontal and vertical translation shift applied to the viewport."
parameters:
  - name: ShiftX, ShiftY
    type: TFloat
    description: "Outputs horizontal and vertical viewport translation offsets."
---

## Description

`GetViewportShift` queries the owner control or event handlers to determine viewport translation offsets.

---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.GetViewportScale
kind: Method
scope: Public
declaration: "procedure GetViewportScale(out ScaleX, ScaleY: TFloat); virtual;"
summary: "Retrieves the current horizontal and vertical scaling factors applied to the viewport."
parameters:
  - name: ScaleX, ScaleY
    type: TFloat
    description: "Outputs horizontal and vertical viewport scaling factors."
---

## Description

`GetViewportScale` queries the owner control or event handlers to determine viewport scaling.

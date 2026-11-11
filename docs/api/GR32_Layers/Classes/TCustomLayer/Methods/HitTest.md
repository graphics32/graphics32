---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.HitTest
kind: Method
scope: Public
declaration: "function HitTest(X, Y: Integer): Boolean;"
summary: "Tests whether the specified viewport coordinates hit the layer."
parameters:
  - name: X, Y
    type: Integer
    description: "Viewport pixel coordinates."
returns:
  - type: Boolean
    description: "True if the coordinates fall inside the active layer region, False otherwise."
---

## Description

`HitTest` executes internal hit-testing and invokes the `OnHitTest` event handler if assigned.

---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnHitTest
kind: Event
scope: Published
declaration: |
  type
    THitTestEvent = procedure(Sender: TObject; X, Y: Integer; var Passed: Boolean) of object;
  
  property OnHitTest: THitTestEvent read ... write ...;
summary: "Fired when hit testing viewport coordinates on the layer."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance being tested."
  - name: X, Y
    type: Integer
    description: "Viewport pixel coordinates."
  - name: Passed
    type: Boolean
    description: "Passed in/out boolean flag indicating whether the point hits the layer."
---

## Description

`OnHitTest` allows custom hit-testing logic for the layer.
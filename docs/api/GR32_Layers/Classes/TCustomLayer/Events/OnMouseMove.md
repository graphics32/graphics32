---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnMouseMove
kind: Event
scope: Published
declaration: "property OnMouseMove: TMouseMoveEvent read ... write ...;"
summary: "Fired when the mouse pointer moves over the layer or captured layer."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance."
  - name: Shift
    type: TShiftState
    description: "Keyboard modifier shift state."
  - name: X, Y
    type: Integer
    description: "Viewport pixel coordinates of the mouse cursor."
---

## Description

`OnMouseMove` handles mouse pointer movement events.

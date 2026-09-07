---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnMouseUp
kind: Event
scope: Published
declaration: "property OnMouseUp: TMouseEvent read ... write ...;"
summary: "Fired when a mouse button is released over the layer."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance."
  - name: Button
    type: TMouseButton
    description: "The mouse button released."
  - name: Shift
    type: TShiftState
    description: "Keyboard modifier shift state."
  - name: X, Y
    type: Integer
    description: "Viewport pixel coordinates of the mouse cursor."
---

## Description

`OnMouseUp` handles mouse button release events.

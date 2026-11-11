---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnMouseDown
kind: Event
scope: Published
declaration: "property OnMouseDown: TMouseEvent read ... write ...;"
summary: "Fired when a mouse button is pressed over the layer."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance."
  - name: Button
    type: TMouseButton
    description: "The mouse button that was pressed (mbLeft, mbRight, mbMiddle)."
  - name: Shift
    type: TShiftState
    description: "Keyboard modifier shift state."
  - name: X, Y
    type: Integer
    description: "Viewport pixel coordinates of the mouse cursor."
---

## Description

`OnMouseDown` handles mouse button press events.

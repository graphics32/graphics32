---
layout: doc
docType: api
unit: GR32_Image
entity: TImgMouseMoveEvent
kind: Type
declaration: |
  TImgMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer; Layer: TCustomLayer) of object;
summary: "Delegate type for layer-aware mouse movement events in image controls."
parameters:
  - name: Sender
    type: TObject
    description: "The image control dispatching the mouse move event."
  - name: Shift
    type: TShiftState
    description: "State of keyboard modifiers (`ssShift`, `ssCtrl`, `ssAlt`) and mouse buttons during movement."
  - name: X, Y
    type: Integer
    description: "Coordinates of the mouse cursor relative to the control buffer."
  - name: Layer
    type: TCustomLayer
    description: "The layer currently hovered under the mouse cursor, or `nil` if no layer is under the cursor."
seealso:
  - "[[TCustomImage32.OnMouseMove|OnMouseMove]]"
---

## Description

`TImgMouseMoveEvent` defines the event handler signature for layer-aware mouse movement events ([[TCustomImage32.OnMouseMove|OnMouseMove]]) in [[TCustomImage32]].

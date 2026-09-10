---
layout: doc
docType: api
unit: GR32_Image
entity: TImgMouseEvent
kind: Type
declaration: |
  TImgMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer; Layer: TCustomLayer) of object;
summary: "Delegate type for layer-aware mouse button events in image controls."
parameters:
  - name: Sender
    type: TObject
    description: "The image control dispatching the mouse event."
  - name: Button
    type: TMouseButton
    description: "The mouse button that was pressed or released (`mbLeft`, `mbRight`, or `mbMiddle`)."
  - name: Shift
    type: TShiftState
    description: "State of keyboard modifiers (`ssShift`, `ssCtrl`, `ssAlt`) and mouse buttons."
  - name: X, Y
    type: Integer
    description: "Coordinates of the mouse cursor relative to the control buffer."
  - name: Layer
    type: TCustomLayer
    description: "The layer located under the mouse cursor, or `nil` if no layer was hit."
seealso:
  - "[[TCustomImage32.OnMouseDown]]"
  - "[[TCustomImage32.OnMouseUp]]"
---

## Description

`TImgMouseEvent` defines the event handler signature for layer-aware mouse button events ([[TCustomImage32.OnMouseDown|OnMouseDown]], [[TCustomImage32.OnMouseUp|OnMouseUp]]) in [[TCustomImage32]].

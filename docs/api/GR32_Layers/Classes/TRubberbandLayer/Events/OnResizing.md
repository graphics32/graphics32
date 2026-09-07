---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandLayer
entity: TRubberbandLayer.OnResizing
kind: Event
scope: Published
declaration: |
  type
    TRBResizingEvent = procedure(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TRBDragState;
      Shift: TShiftState) of object;
  
  property OnResizing: TRBResizingEvent read ... write ...;
summary: "Fired continuously during layer resizing."
parameters:
  - name: Sender
    type: TObject
    description: "The rubberband layer instance."
  - name: OldLocation
    type: TFloatRect
    description: "Previous location rectangle."
  - name: NewLocation
    type: TFloatRect
    description: "Var parameter specifying new location rectangle."
  - name: DragState
    type: TRBDragState
    description: "Active drag state handle."
  - name: Shift
    type: TShiftState
    description: "Keyboard modifier shift state."
---

## Description

`OnResizing` is fired during interactive layer move or resize.
---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandLayer
entity: TRubberbandLayer.OnConstrain
kind: Event
scope: Published
declaration: |
  type
    TRBConstrainEvent = procedure(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TRBDragState; 
      Shift: TShiftState) of object;
  
  property OnConstrain: TRBConstrainEvent read ... write ...;
summary: "Fired when roConstrained is set in Options to enforce custom layer position bounds."
parameters:
  - name: Sender
    type: TObject
    description: "The rubberband layer instance."
  - name: OldLocation
    type: TFloatRect
    description: "Previous location rectangle before resizing."
  - name: NewLocation
    type: TFloatRect
    description: "Var parameter specifying candidate location rectangle."
  - name: DragState
    type: TRBDragState
    description: "Active drag state handle."
  - name: Shift
    type: TShiftState
    description: "Keyboard modifier shift state."
---

## Description

`OnConstrain` allows enforcing custom spatial constraints on `NewLocation`. The event is fired during interactive sizing when [[TRBOptions.roConstrained|roConstrained]] is specified in the layer [[Options]].

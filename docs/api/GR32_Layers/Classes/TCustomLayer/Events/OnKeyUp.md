---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnKeyUp
kind: Event
scope: Published
declaration: "property OnKeyUp: TKeyEvent read ... write ...;"
summary: "Fired when a key is released while the layer has focus."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance."
  - name: Key
    type: Word
    description: "Virtual key code."
  - name: Shift
    type: TShiftState
    description: "Keyboard modifier shift state."
---

## Description

`OnKeyUp` handles keyboard release events routed to the active layer.

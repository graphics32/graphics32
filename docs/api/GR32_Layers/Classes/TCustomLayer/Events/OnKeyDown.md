---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnKeyDown
kind: Event
scope: Published
declaration: "property OnKeyDown: TKeyEvent read ... write ...;"
summary: "Fired when a key is pressed while the layer has focus."
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

`OnKeyDown` handles keyboard press events routed to the active layer.

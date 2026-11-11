---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.OnUserChange
kind: Event
scope: Published
declaration: "property OnUserChange: TNotifyEvent read ... write ...;"
summary: "Fired whenever user interactive dragging modifies layer coordinates or handles."
parameters:
  - name: Sender
    type: TObject
    description: "The rubberband layer instance."
---

## Description

`OnUserChange` is triggered when *interactive* user editing changes layer geometry or handle coordinates; Explicitly changing `Location` in code does not cause the event to fire.

---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.OnHandleClicked
kind: Event
scope: Published
declaration: |
  type
    TRubberBandHandleEvent = procedure(Sender: TCustomRubberBandLayer; AIndex: Integer) of object;
  
  property OnHandleClicked: TRubberBandHandleEvent read ... write ...;
summary: "Fired when a selection handle or vertex is clicked."
parameters:
  - name: Sender
    type: TCustomRubberBandLayer
    description: "The rubberband layer instance."
  - name: AIndex
    type: Integer
    description: "Zero-based index of the handle clicked (-1 if body clicked)."
---

## Description

`OnHandleClicked` is fired when the user clicks a handle.
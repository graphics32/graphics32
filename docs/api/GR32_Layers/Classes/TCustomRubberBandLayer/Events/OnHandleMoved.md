---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.OnHandleMoved
kind: Event
scope: Published
declaration: |
  type
    TRubberBandHandleEvent = procedure(Sender: TCustomRubberBandLayer; AIndex: Integer) of object;
  
  property OnHandleMoved: TRubberBandHandleEvent read ... write ...;
summary: "Fired after a handle position has been updated during dragging."
parameters:
  - name: Sender
    type: TCustomRubberBandLayer
    description: "The rubberband layer instance."
  - name: AIndex
    type: Integer
    description: "Zero-based index of the moved handle."
---

## Description

`OnHandleMoved` is triggered after a handle position update.

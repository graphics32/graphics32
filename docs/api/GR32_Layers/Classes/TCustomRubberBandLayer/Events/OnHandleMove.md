---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.OnHandleMove
kind: Event
scope: Published
declaration: |
  type
    TRubberBandHandleMoveEvent = procedure(Sender: TCustomRubberBandLayer; AIndex: Integer;
      var APos: TFloatPoint) of object;
  
  property OnHandleMove: TRubberBandHandleMoveEvent read ... write ...;
summary: "Fired continuously during handle dragging to allow customizing new vertex coordinates."
parameters:
  - name: Sender
    type: TCustomRubberBandLayer
    description: "The rubberband layer instance."
  - name: AIndex
    type: Integer
    description: "Zero-based index of the vertex being moved."
  - name: APos
    type: TFloatPoint
    description: "Var parameter specifying target floating-point coordinates."
---

## Description

`OnHandleMove` allows modifying handle movement coordinates in real time.

---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.OnUpdateHandle
kind: Event
scope: Published
declaration: |
  type
    TRubberBandUpdateHandleEvent = procedure(Sender: TCustomRubberBandLayer; Buffer: TBitmap32;
      const p: TFloatPoint; AIndex: Integer; var UpdateRect: TRect;
      var Handled: Boolean) of object;
  
  property OnUpdateHandle: TRubberBandUpdateHandleEvent read ... write ...;
summary: "Fired when calculating invalidation update rectangles for a handle."
parameters:
  - name: Sender
    type: TCustomRubberBandLayer
    description: "The rubberband layer instance."
  - name: Buffer
    type: TBitmap32
    description: "Target bitmap buffer."
  - name: p
    type: TFloatPoint
    description: "Handle center position."
  - name: AIndex
    type: Integer
    description: "Zero-based handle index."
  - name: UpdateRect
    type: TRect
    description: "Var parameter specifying invalidated rectangle."
  - name: Handled
    type: Boolean
    description: "Set to True if custom update rectangle calculation handled the update."
---

## Description

`OnUpdateHandle` enables customizion of the handle invalidation areas.

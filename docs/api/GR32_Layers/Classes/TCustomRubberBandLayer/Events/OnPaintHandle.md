---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.OnPaintHandle
kind: Event
scope: Published
declaration: |
  type
    TRubberBandPaintHandleEvent = procedure(Sender: TCustomRubberBandLayer; Buffer: TBitmap32;
      const p: TFloatPoint; AIndex: Integer; var ADrawParams: TRubberBandHandleDrawParams;
      var Handled: Boolean) of object;
  
  property OnPaintHandle: TRubberBandPaintHandleEvent read ... write ...;
summary: "Fired when rendering a handle, allowing custom handle drawing."
parameters:
  - name: Sender
    type: TCustomRubberBandLayer
    description: "The rubberband layer instance."
  - name: Buffer
    type: TBitmap32
    description: "Target bitmap rendering buffer."
  - name: p
    type: TFloatPoint
    description: "Handle center position in viewport coordinates."
  - name: AIndex
    type: Integer
    description: "Zero-based index of the handle."
  - name: ADrawParams
    type: TRubberBandHandleDrawParams
    description: "Var parameter containing handle styling parameters."
  - name: Handled
    type: Boolean
    description: "Set to True if custom rendering handled the handle draw."
---

## Description

`OnPaintHandle` allows custom rendering of individual handles.

---
layout: doc
docType: api
unit: GR32_Layers
entity: TRubberBandHandleDrawParams
kind: Type
declaration: |
  TRubberBandHandleDrawParams = record
    HandleStyle: TRubberBandHandleStyle;
    HandleSize: TFloat;
    HandleFill: TColor32;
    HandleFrame: TColor32;
    HandleFrameSize: TFloat;
  end;
summary: "Drawing parameters controlling handle rendering on rubberband layers."
---

## Description

`TRubberBandHandleDrawParams` contains formatting parameters passed to custom handle paint routines (`TRubberBandPaintHandleEvent`).

## Fields

| Field | Type | Description |
| --- | --- | --- |
| `HandleStyle` | `TRubberBandHandleStyle` | Shape style of the handle (`hsSquare`, `hsCircle`, or `hsDiamond`). |
| `HandleSize` | `TFloat` | Radius/half-width of the handle in pixels. |
| `HandleFill` | `TColor32` | Interior fill color of the handle. |
| `HandleFrame` | `TColor32` | Outline/frame color of the handle. |
| `HandleFrameSize` | `TFloat` | Width of the handle frame outline in pixels. |

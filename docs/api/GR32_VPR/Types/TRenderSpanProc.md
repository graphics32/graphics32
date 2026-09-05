---
layout: doc
docType: api
unit: GR32_VPR
entity: TRenderSpanProc
kind: Type
aliases: [TRenderSpanEvent]
declaration: |
  TRenderSpanProc = procedure(Data: Pointer; const Span: TValueSpan; DstY: Integer);
  TRenderSpanEvent = procedure(const Span: TValueSpan; DstY: Integer) of object;
summary: "Procedural callback types for receiving rasterized horizontal coverage spans."
seealso:
  - "[[TValueSpan]]"
  - "[[RenderPolygon]]"
  - "[[RenderPolyPolygon]]"
---

## Description

`TRenderSpanProc` and `TRenderSpanEvent` define the function signatures for callbacks passed to [[RenderPolygon]] and [[RenderPolyPolygon]].<br>
When the VPR engine completes analytical integration for a scanline, it calls the registered callback with the scanline's `TValueSpan` data and Y-coordinate (`DstY`).

### Parameters

| Parameter | Type | Description |
| --- | --- | --- |
| `Data` | `Pointer` | User-defined context pointer passed through from `RenderPolygon` or `RenderPolyPolygon` (used only with `TRenderSpanProc`). |
| `Span` | `TValueSpan` | Horizontal span containing active coverage values (`Values`) and bounds (`LowX`, `HighX`). |
| `DstY` | `Integer` | The 0-based target scanline Y-coordinate. |

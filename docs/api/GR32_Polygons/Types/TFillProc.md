---
layout: doc
docType: api
unit: GR32_Polygons
entity: TFillProc
kind: Type
declaration: "TFillProc = procedure(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: Integer; Color: TColor32);"
summary: "Procedural pointer type for internal alpha coverage span calculation functions."
---

## Description

`TFillProc` represents a function prototype used internally by `TPolygonRenderer32VPR` and optimized assembly/SSE routines to map floating-point polygon edge coverages into 32-bit ARGB alpha color values across a single scanline span.

### Parameters

| Name | Type | Description |
| --- | --- | --- |
| `Coverage` | `PSingleArray` | Pointer to array of floating-point coverage winding values for the span. |
| `AlphaValues` | `PColor32Array` | Output array pointer where computed 32-bit ARGB colors/alphas are written. |
| `Count` | `Integer` | Number of pixels in the span. |
| `Color` | `TColor32` | Base fill color. |

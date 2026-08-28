---
layout: doc
docType: api
unit: GR32_Polygons
entity: TFillLineEvent
kind: Type
declaration: "TFillLineEvent = procedure(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode) of object;"
summary: "Method event signature for span rendering in custom polygon fillers."
---

## Description

`TFillLineEvent` defines the callback method signature used by custom polygon fillers (`TCustomPolygonFiller`) to paint individual horizontal scanline spans.

### Parameters

| Name | Type | Description |
| --- | --- | --- |
| `Dst` | `PColor32` | Pointer to the first destination pixel in the bitmap scanline. |
| `DstX` | `Integer` | X-coordinate of the start of the scanline span. |
| `DstY` | `Integer` | Y-coordinate of the scanline span. |
| `Length` | `Integer` | Number of pixels in the scanline span. |
| `AlphaValues` | `PColor32` | Pointer to per-pixel coverage alpha values (or `nil` if full coverage). |
| `CombineMode` | `TCombineMode` | Combine mode specified by the destination bitmap. |

---
layout: doc
docType: api
unit: GR32_VPR
entity: TValueSpan
kind: Type
declaration: |
  TValueSpan = record
    LowX, HighX: Integer;
    Values: PSingleArray;
  end;
summary: "Record representing a horizontal span of analytical coverage values for a single scanline."
seealso:
  - "[[RenderPolygon]]"
  - "[[RenderPolyPolygon]]"
  - "[[TRenderSpanProc]]"
  - "[[TRenderSpanEvent]]"
  - "[Vectorial Polygon Rasterizer](/guide/vpr) guide"
---

## Description

`TValueSpan` describes a horizontal segment of pixels on a specific scanline where polygon coverage values have been computed by the VPR rasterizer.

### Fields

| Field | Type | Description |
| --- | --- | --- |
| `LowX` | `Integer` | Minimum active 0-based pixel X-coordinate on the scanline for this span. |
| `HighX` | `Integer` | Maximum active 0-based pixel X-coordinate on the scanline for this span. |
| `Values` | `PSingleArray` | Pointer to the array of floating-point coverage winding values indexed starting at `0` (corresponding to pixel `LowX`). The total number of valid elements is `HighX - LowX + 1`. |

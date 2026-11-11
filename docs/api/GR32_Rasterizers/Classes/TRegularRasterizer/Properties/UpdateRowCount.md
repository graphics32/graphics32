---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TRegularRasterizer
entity: TRegularRasterizer.UpdateRowCount
kind: Property
declaration: "property UpdateRowCount: Integer read FUpdateRowCount write FUpdateRowCount;"
summary: "Determines the number of rasterized scanline rows processed before firing bitmap change notifications."
---

## Description

`UpdateRowCount` specifies how frequently `TRegularRasterizer` triggers `Dst.Changed` area update notifications during scanline rendering.

- If `UpdateRowCount = 0` (default), change notifications are issued once after the entire destination rectangle is rasterized.
- If `UpdateRowCount > 0`, notifications are issued in batches every `UpdateRowCount` scanlines, allowing progressive visual updates on GUI surfaces during long operations.

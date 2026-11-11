---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TProgressiveRasterizer
entity: TProgressiveRasterizer.UpdateRows
kind: Property
declaration: "property UpdateRows: Boolean read FUpdateRows write SetUpdateRows default True;"
summary: "Determines whether bitmap change notifications are fired row-by-row or after completing entire passes."
---

## Description

`UpdateRows` specifies the granularity of `OnAreaChanged` notifications sent to the destination bitmap during progressive rendering.

- When `True` (default), notifications are sent row-by-row as each row of blocks is filled, producing a smooth progressive sweep on screen.
- When `False`, notifications are sent only after each full refinement pass finishes.

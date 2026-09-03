---
layout: doc
docType: api
unit: GR32_Brushes
parent: TStrokeBrush
entity: TStrokeBrush.MiterLimit
kind: Property
declaration: "property MiterLimit: TFloat read FMiterLimit write SetMiterLimit;"
summary: "Maximum ratio of miter join length to stroke width before clipping miter corners to bevels."
---

## Description

`MiterLimit` sets the maximum allowable miter ratio when `JoinStyle` is `jsMiter`. Extremely sharp corner angles exceeding `MiterLimit` are automatically truncated to bevel joins.

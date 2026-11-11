---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.FrameStipple
kind: Property
scope: Public
declaration: "property FrameStipple: TArrayOfColor32 read FFrameStipplePattern write SetFrameStipplePattern;"
summary: "Array of colors defining the dashed/stippled frame pattern."
---

## Description

`FrameStipple` defines the color pattern used when rendering the stippled selection frame.<br>
The default value is `[clWhite32, clWhite32, clBlack32, clBlack32]`.

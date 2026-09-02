---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TDraftRasterizer
entity: TDraftRasterizer.PixelSize
kind: Property
declaration: "property PixelSize: Integer read FPixelSize write SetPixelSize default 4;"
summary: "Specifies the width and height in pixels of filled draft blocks."
---

## Description

`PixelSize` defines the dimension (in pixels) of square blocks rendered by `TDraftRasterizer`.

A single sample is taken at the top-left corner of each block and filled across the entire [[PixelSize]] $\times$ [[PixelSize]] rectangle. Larger values produce coarser previews with higher rendering performance.

---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TRasterizer
entity: TRasterizer.Assign
kind: Method
declaration: "procedure Assign(Source: TPersistent); override;"
summary: "Copies properties from another TRasterizer or extracts combination settings from a TCustomBitmap32 instance."
parameters:
  - name: Source
    type: TPersistent
    description: "Source object from which properties or combination settings are copied."
---

## Description

`Assign` copies property values from `Source` into this rasterizer instance.

If `Source` is a [[TCustomBitmap32]], `Assign` extracts pixel combination settings (`MasterAlpha`, `DrawMode`, `CombineMode`, `OnPixelCombine`, `OuterColor`) via [[CombineInfo]] and configures internal blending delegates accordingly.

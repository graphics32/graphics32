---
layout: doc
docType: api
unit: GR32_Transforms
entity: SetBorderTransparent
kind: Procedure
declaration: "procedure SetBorderTransparent(ABitmap: TCustomBitmap32; ARect: TRect);"
summary: "Sets the alpha channel of border pixels in a rectangle to zero (transparent)."
parameters:
  - name: ABitmap
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: ARect
    type: TRect
    description: "Rectangle whose outer border pixels will be cleared to zero alpha."
---

## Description

`SetBorderTransparent` clears the alpha component ($00) along the outer boundary pixels of `ARect` inside `ABitmap`, preventing edge sampling artifacts when resampling transformed bitmaps.

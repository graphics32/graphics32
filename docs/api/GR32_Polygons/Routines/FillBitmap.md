---
layout: doc
docType: api
unit: GR32_Polygons
entity: FillBitmap
kind: Function
declaration: "procedure FillBitmap(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller);"
summary: "Fills an entire destination bitmap buffer using a specified custom polygon filler."
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Filler
    type: TCustomPolygonFiller
    description: "Custom polygon filler instance."
---

## Description

`FillBitmap` iterates over all scanlines of `Bitmap` and delegates pixel color generation across the entire bitmap width to `Filler.FillLine`.

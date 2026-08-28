---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolygonXS_LCD2
kind: Function
declaration: "procedure PolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);"
summary: "Rasterizes a fixed-point polygon using soft LCD sub-pixel antialiasing."
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: TArrayOfFixedPoint
    description: "Fixed-point polygon vertices."
  - name: Color
    type: TColor32
    description: "Fill color."
  - name: FillMode
    type: TPolyFillMode
    description: "Fill rule."
  - name: Transformation
    type: TTransformation
    description: "Optional transformation."
---

## Description

`PolygonXS_LCD2` rasterizes a single fixed-point polygon using soft LCD sub-pixel antialiasing.

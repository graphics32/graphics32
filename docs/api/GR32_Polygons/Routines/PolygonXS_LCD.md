---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolygonXS_LCD
kind: Function
declaration: "procedure PolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);"
summary: "Rasterizes a fixed-point polygon using 3x LCD sub-pixel antialiasing."
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

`PolygonXS_LCD` rasterizes a single fixed-point polygon using 3x LCD sub-pixel antialiasing.

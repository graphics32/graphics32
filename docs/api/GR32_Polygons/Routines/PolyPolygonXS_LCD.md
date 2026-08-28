---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolygonXS_LCD
kind: Function
declaration: "procedure PolyPolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
summary: "Rasterizes multi-contour fixed-point polygons using 3x LCD sub-pixel antialiasing."
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: TArrayOfArrayOfFixedPoint
    description: "Fixed-point polygon contours."
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

`PolyPolygonXS_LCD` rasterizes multi-contour fixed-point polygons using 3x LCD sub-pixel antialiasing.

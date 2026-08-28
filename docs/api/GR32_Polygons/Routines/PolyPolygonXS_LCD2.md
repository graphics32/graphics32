---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolygonXS_LCD2
kind: Function
declaration: "procedure PolyPolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
summary: "Rasterizes multi-contour fixed-point polygons using soft LCD sub-pixel antialiasing."
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

`PolyPolygonXS_LCD2` rasterizes multi-contour fixed-point polygons using soft LCD sub-pixel antialiasing transitions.

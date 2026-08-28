---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolygonXS
kind: Function
summary: "Rasterizes multi-contour fixed-point polygons onto a destination bitmap."
overloads:
  - signature: "procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour fixed-point polygons onto Bitmap using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
      - name: Color
        type: TColor32
        description: "Solid fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour fixed-point polygons onto Bitmap using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."
---

## Description

`PolyPolygonXS` converts fixed-point polygon vertices to floating-point coordinates and rasterizes them using `PolyPolygonFS`.

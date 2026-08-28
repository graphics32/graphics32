---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolygonXS
kind: Function
summary: "Rasterizes a single fixed-point polygon shape onto a destination bitmap."
overloads:
  - signature: "procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a fixed-point polygon onto Bitmap using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
      - name: Color
        type: TColor32
        description: "Solid fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a fixed-point polygon onto Bitmap using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
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

`PolygonXS` converts fixed-point polygon vertices to floating-point coordinates and rasterizes them using `PolygonFS`.

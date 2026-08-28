---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolygonFS
kind: Function
summary: "Rasterizes a single floating-point polygon shape onto a destination bitmap."
overloads:
  - signature: "procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a floating-point polygon onto Bitmap using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Color
        type: TColor32
        description: "Solid fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a floating-point polygon onto Bitmap using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a floating-point polygon onto Bitmap clipped to ClipRect using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: ClipRect
        type: TRect
        description: "Clipping rectangle."
      - name: Color
        type: TColor32
        description: "Solid fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a floating-point polygon onto Bitmap clipped to ClipRect using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: ClipRect
        type: TRect
        description: "Clipping rectangle."
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

`PolygonFS` rasterizes a single floating-point polygon shape onto a destination bitmap.

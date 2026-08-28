---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolygonFS
kind: Function
summary: "Rasterizes multi-contour floating-point polygons onto a destination bitmap."
overloads:
  - signature: "procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour floating-point polygons onto Bitmap using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours."
      - name: Color
        type: TColor32
        description: "Solid fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour floating-point polygons onto Bitmap using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: FillMode
        type: TPolyFillMode
        description: "Polygon fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour floating-point polygons onto Bitmap clipped to ClipRect using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours."
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

  - signature: "procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour floating-point polygons onto Bitmap clipped to ClipRect using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours."
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

`PolyPolygonFS` rasterizes multi-contour floating-point polygons using coverage-based antialiasing.

---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolygonFS_LCD2
kind: Function
summary: "Rasterizes multi-contour floating-point polygons using soft LCD sub-pixel antialiasing."
overloads:
  - signature: "procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour floating-point polygons using soft LCD sub-pixel antialiasing."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours."
      - name: Color
        type: TColor32
        description: "Fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes multi-contour floating-point polygons clipped to ClipRect using soft LCD sub-pixel antialiasing."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours."
      - name: ClipRect
        type: TRect
        description: "Clipping rectangle."
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

`PolyPolygonFS_LCD2` rasterizes multi-contour floating-point polygons using soft LCD sub-pixel antialiasing transitions.

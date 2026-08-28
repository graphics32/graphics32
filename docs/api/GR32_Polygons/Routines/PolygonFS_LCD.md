---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolygonFS_LCD
kind: Function
summary: "Rasterizes a single floating-point polygon using 3x LCD sub-pixel antialiasing."
overloads:
  - signature: "procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a floating-point polygon using LCD sub-pixel antialiasing."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Color
        type: TColor32
        description: "Fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;"
    summary: "Rasterizes a floating-point polygon clipped to ClipRect using LCD sub-pixel antialiasing."
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
        description: "Fill color."
      - name: FillMode
        type: TPolyFillMode
        description: "Fill rule."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."
---

## Description

`PolygonFS_LCD` rasterizes a single floating-point polygon using LCD sub-pixel antialiasing.

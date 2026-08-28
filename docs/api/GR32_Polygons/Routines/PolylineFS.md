---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolylineFS
kind: Function
summary: "Renders a single stroked floating-point polyline onto a bitmap."
overloads:
  - signature: "procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;"
    summary: "Renders a stroked floating-point polyline using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: Color
        type: TColor32
        description: "Stroke color."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: StrokeWidth
        type: TFloat
        description: "Line width in pixels."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFloat
        description: "Miter ratio limit."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;"
    summary: "Renders a stroked floating-point polyline using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: StrokeWidth
        type: TFloat
        description: "Line width in pixels."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFloat
        description: "Miter ratio limit."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."
---

## Description

`PolylineFS` converts a single polyline into a multi-contour array and delegates stroking to `PolyPolylineFS`.

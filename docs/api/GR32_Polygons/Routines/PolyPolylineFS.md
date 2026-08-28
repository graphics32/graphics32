---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolylineFS
kind: Function
summary: "Renders stroked floating-point polylines with configurable join styles, cap styles, and line widths."
overloads:
  - signature: "procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;"
    summary: "Renders stroked floating-point polylines using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polyline vertex contours."
      - name: Color
        type: TColor32
        description: "Stroke color."
      - name: Closed
        type: Boolean
        description: "True if polylines are closed loops."
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
        description: "Miter ratio limit for sharp corners."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;"
    summary: "Renders stroked floating-point polylines using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polyline vertex contours."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: Closed
        type: Boolean
        description: "True if polylines are closed loops."
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
        description: "Miter ratio limit for sharp corners."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."
---

## Description

`PolyPolylineFS` converts polyline paths into stroked outline polygon contours using `BuildPolyPolyLine` and renders them with `PolyPolygonFS`.

---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolylineXS
kind: Function
summary: "Renders a single stroked fixed-point polyline onto a bitmap."
overloads:
  - signature: "procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;"
    summary: "Renders a single stroked fixed-point polyline using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: Color
        type: TColor32
        description: "Stroke color."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: StrokeWidth
        type: TFixed
        description: "Stroke width in 16.16 fixed point format."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFixed
        description: "Miter limit in 16.16 fixed point format."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."

  - signature: "procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;"
    summary: "Renders a single stroked fixed-point polyline using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: StrokeWidth
        type: TFixed
        description: "Stroke width in 16.16 fixed point format."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFixed
        description: "Miter limit in 16.16 fixed point format."
      - name: Transformation
        type: TTransformation
        description: "Optional transformation."
---

## Description

`PolylineXS` renders a single fixed-point polyline by delegating to `PolyPolylineXS`.

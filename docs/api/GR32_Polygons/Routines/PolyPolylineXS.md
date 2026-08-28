---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolylineXS
kind: Function
summary: "Renders stroked fixed-point polylines onto a bitmap."
overloads:
  - signature: "procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;"
    summary: "Renders stroked fixed-point polylines using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polyline contours."
      - name: Color
        type: TColor32
        description: "Stroke color."
      - name: Closed
        type: Boolean
        description: "True if polylines are closed loops."
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

  - signature: "procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;"
    summary: "Renders stroked fixed-point polylines using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polyline contours."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: Closed
        type: Boolean
        description: "True if polylines are closed loops."
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

`PolyPolylineXS` builds stroke outline polygons from fixed-point polyline paths and renders them with `PolyPolygonXS`.

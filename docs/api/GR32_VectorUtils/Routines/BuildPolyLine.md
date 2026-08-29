---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: BuildPolyLine
kind: Function
summary: "Generates a stroked outline polygon contour from a polyline path."
overloads:
  - signature: "function BuildPolyLine(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;"
    summary: "Builds a floating-point stroked outline polygon using PolylineBuilder."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: StrokeWidth
        type: TFloat
        description: "Stroke line width in pixels."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFloat
        description: "Miter ratio limit."

  - signature: "function BuildPolyLine(const Points: TArrayOfFixedPoint; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload;"
    summary: "Builds a fixed-point stroked outline polygon using PolylineBuilder."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: StrokeWidth
        type: TFixed
        description: "Stroke line width."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFixed
        description: "Miter ratio limit."
---

## Description

`BuildPolyLine` delegates stroke outline polygon construction to the active `PolylineBuilder` backend.

---
layout: doc
docType: api
unit: GR32_VectorUtils
parent: TPolyLineBuilder
entity: TPolyLineBuilder.BuildPolyLine
kind: Method
summary: "Generates a stroked outline polygon from a single polyline path."
overloads:
  - signature: "class function BuildPolyLine(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; virtual; abstract;"
    summary: "Builds a floating-point stroked outline polygon contour."
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
        description: "Miter ratio limit for sharp corners."

  - signature: "class function BuildPolyLine(const Points: TArrayOfFixedPoint; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload; virtual;"
    summary: "Builds a fixed-point stroked outline polygon contour."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Polyline vertices."
      - name: StrokeWidth
        type: TFixed
        description: "Stroke line width in fixed point format."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: EndStyle
        type: TEndStyle
        description: "Line end cap style."
      - name: MiterLimit
        type: TFixed
        description: "Miter ratio limit in fixed point format."
---

## Description

`BuildPolyLine` constructs an expanded stroke boundary polygon covering the polyline path specified by `Points`.

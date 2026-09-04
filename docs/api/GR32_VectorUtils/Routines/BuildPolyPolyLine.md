---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: BuildPolyPolyLine
kind: Function
summary: "Generates stroked outline polygon contours from multi-contour polyline paths."
overloads:
  - signature: "function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Builds floating-point stroked outline polygons from multi-contour paths using PolylineBuilder."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polyline contours."
      - name: Closed
        type: Boolean
        description: "True if paths are closed loops."
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

    returns:
      - type: TArrayOfArrayOfFloatPoint
        description: "A [[TArrayOfArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function BuildPolyPolyLine(const Points: TArrayOfArrayOfFixedPoint; Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Builds fixed-point stroked outline polygons from multi-contour paths using PolylineBuilder."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Array of polyline contours."
      - name: Closed
        type: Boolean
        description: "True if paths are closed loops."
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

    returns:
      - type: TArrayOfArrayOfFixedPoint
        description: "A [[TArrayOfArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`BuildPolyPolyLine` delegates stroke outline polygon construction for multi-contour paths to `PolylineBuilder`.

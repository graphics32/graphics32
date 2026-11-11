---
layout: doc
docType: api
unit: GR32_VectorUtils
parent: TPolyLineBuilder
entity: TPolyLineBuilder.BuildPolyPolyLine
kind: Method
summary: "Generates stroked outline polygons from multi-contour polyline paths."
overloads:
  - signature: "class function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint; overload; virtual; abstract;"
    summary: "Builds floating-point stroked outline polygon contours from multi-contour paths."
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
        description: "Miter ratio limit for sharp corners."

    returns:
      - type: TArrayOfArrayOfFloatPoint
        description: "A [[TArrayOfArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "class function BuildPolyPolyLine(const Points: TArrayOfArrayOfFixedPoint; Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfArrayOfFixedPoint; overload; virtual;"
    summary: "Builds fixed-point stroked outline polygon contours from multi-contour paths."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Array of polyline contours."
      - name: Closed
        type: Boolean
        description: "True if paths are closed loops."
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

    returns:
      - type: TArrayOfArrayOfFixedPoint
        description: "A [[TArrayOfArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`BuildPolyPolyLine` constructs stroked outline polygons for multiple polyline paths.

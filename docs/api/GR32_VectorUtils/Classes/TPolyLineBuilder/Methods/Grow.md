---
layout: doc
docType: api
unit: GR32_VectorUtils
parent: TPolyLineBuilder
entity: TPolyLineBuilder.Grow
kind: Method
summary: "Inflates or deflates polygon boundaries by a specified delta offset distance."
overloads:
  - signature: "class function Grow(const Points: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; virtual;"
    summary: "Offsets floating-point polygon vertices by Delta distance."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Delta
        type: TFloat
        description: "Offset distance (positive to inflate, negative to deflate)."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: Closed
        type: Boolean
        description: "True if polygon is a closed loop."
      - name: MiterLimit
        type: TFloat
        description: "Miter ratio limit."

  - signature: "class function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; virtual; abstract;"
    summary: "Offsets floating-point polygon vertices using precomputed vertex normal vectors."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Normals
        type: TArrayOfFloatPoint
        description: "Precomputed vertex unit normal vectors."
      - name: Delta
        type: TFloat
        description: "Offset distance."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: Closed
        type: Boolean
        description: "True if polygon is a closed loop."
      - name: MiterLimit
        type: TFloat
        description: "Miter ratio limit."

  - signature: "class function Grow(const Points: TArrayOfFixedPoint; const Delta: TFixed; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload; virtual;"
    summary: "Offsets fixed-point polygon vertices by Delta distance."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Polygon vertices."
      - name: Delta
        type: TFixed
        description: "Offset distance in fixed point format."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: Closed
        type: Boolean
        description: "True if polygon is a closed loop."
      - name: MiterLimit
        type: TFixed
        description: "Miter ratio limit in fixed point format."

  - signature: "class function Grow(const Points: TArrayOfFixedPoint; const Normals: TArrayOfFixedPoint; const Delta: TFixed; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload; virtual;"
    summary: "Offsets fixed-point polygon vertices using precomputed vertex normal vectors."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Polygon vertices."
      - name: Normals
        type: TArrayOfFixedPoint
        description: "Precomputed vertex unit normal vectors."
      - name: Delta
        type: TFixed
        description: "Offset distance."
      - name: JoinStyle
        type: TJoinStyle
        description: "Corner join style."
      - name: Closed
        type: Boolean
        description: "True if polygon is a closed loop."
      - name: MiterLimit
        type: TFixed
        description: "Miter ratio limit in fixed point format."
---

## Description

`Grow` expands or shrinks a polygon boundary outward or inward by `Delta` distance.

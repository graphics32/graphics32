---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Grow
kind: Function
summary: "Inflates or deflates polygon boundaries by a specified offset distance."
overloads:
  - signature: "function Grow(const Points: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;"
    summary: "Inflates or deflates floating-point polygon vertices by Delta distance."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
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

  - signature: "function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;"
    summary: "Inflates or deflates floating-point polygon vertices using precomputed vertex normal vectors."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Normals
        type: TArrayOfFloatPoint
        description: "Precomputed vertex normal vectors."
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

  - signature: "function Grow(const Points: TArrayOfFixedPoint; const Delta: TFixed; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload;"
    summary: "Inflates or deflates fixed-point polygon vertices by Delta distance."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
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
        description: "Miter ratio limit."

  - signature: "function Grow(const Points: TArrayOfFixedPoint; const Normals: TArrayOfFixedPoint; const Delta: TFixed; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload;"
    summary: "Inflates or deflates fixed-point polygon vertices using precomputed vertex normal vectors."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
      - name: Normals
        type: TArrayOfFixedPoint
        description: "Precomputed vertex normal vectors."
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
        description: "Miter ratio limit."
---

## Description

`Grow` expands or shrinks polygon boundaries outward or inward by `Delta` distance, delegating to `PolylineBuilder.Grow`.

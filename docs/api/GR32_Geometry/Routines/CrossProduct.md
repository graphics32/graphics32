---
layout: doc
docType: api
unit: GR32_Geometry
entity: CrossProduct
kind: Function
summary: "Computes the 2D cross product scalar (perpendicular dot product) of two vectors."
overloads:
  - signature: "function CrossProduct(const V1, V2: TFloatPoint): TFloat; overload;"
    summary: "Computes the 2D cross product of two floating-point vectors."
    parameters:
      - name: V1
        type: TFloatPoint
        description: "First vector."
      - name: V2
        type: TFloatPoint
        description: "Second vector."
  - signature: "function CrossProduct(const V1, V2: TFixedPoint): TFixed; overload;"
    summary: "Computes the 2D cross product of two fixed-point vectors."
    parameters:
      - name: V1
        type: TFixedPoint
        description: "First vector."
      - name: V2
        type: TFixedPoint
        description: "Second vector."
  - signature: "function CrossProduct(const V1, V2: TPoint): Integer; overload;"
    summary: "Computes the 2D cross product of two integer vectors."
    parameters:
      - name: V1
        type: TPoint
        description: "First vector."
      - name: V2
        type: TPoint
        description: "Second vector."
---

## Description

`CrossProduct` calculates the 2D cross product scalar of vectors $V1$ and $V2$, defined as:

$$\text{CrossProduct}(V1, V2) = V1.X \cdot V2.Y - V1.Y \cdot V2.X$$

The cross product magnitude indicates the relative orientation of vector $V2$ relative to $V1$:
- A positive result indicates a counter-clockwise turn from $V1$ to $V2$.
- A negative result indicates a clockwise turn.
- A zero result indicates collinear vectors.

---
layout: doc
docType: api
unit: GR32_Geometry
entity: Dot
kind: Function
summary: "Computes the dot product (scalar product) of two vectors."
overloads:
  - signature: "function Dot(const V1, V2: TFloatPoint): TFloat; overload;"
    summary: "Computes the dot product of two floating-point vectors."
    parameters:
      - name: V1
        type: TFloatPoint
        description: "First vector."
      - name: V2
        type: TFloatPoint
        description: "Second vector."
  - signature: "function Dot(const V1, V2: TFixedPoint): TFixed; overload;"
    summary: "Computes the dot product of two fixed-point vectors."
    parameters:
      - name: V1
        type: TFixedPoint
        description: "First vector."
      - name: V2
        type: TFixedPoint
        description: "Second vector."
  - signature: "function Dot(const V1, V2: TPoint): Integer; overload;"
    summary: "Computes the dot product of two integer vectors."
    parameters:
      - name: V1
        type: TPoint
        description: "First vector."
      - name: V2
        type: TPoint
        description: "Second vector."
---

## Description

`Dot` calculates the dot product (also known as scalar product) of two 2D vectors $V1$ and $V2$, defined as:

$$\text{Dot}(V1, V2) = V1.X \cdot V2.X + V1.Y \cdot V2.Y$$

The dot product measures vector alignment:
- Positive when vectors point in a similar direction ($< 90^\circ$ angle).
- Zero when vectors are orthogonal ($90^\circ$ angle).
- Negative when vectors point in opposite directions ($> 90^\circ$ angle).

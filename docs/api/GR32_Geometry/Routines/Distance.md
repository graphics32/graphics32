---
layout: doc
docType: api
unit: GR32_Geometry
entity: Distance
kind: Function
summary: "Calculates the Euclidean distance between two points."
overloads:
  - signature: "function Distance(const V1, V2: TFloatPoint): TFloat; overload;"
    summary: "Calculates Euclidean distance between two floating-point coordinates."
    parameters:
      - name: V1
        type: TFloatPoint
        description: "First point."
      - name: V2
        type: TFloatPoint
        description: "Second point."
    returns:
      - type: TFloat
        description: "The Euclidean distance between V1 and V2."
  - signature: "function Distance(const V1, V2: TFixedPoint): TFixed; overload;"
    summary: "Calculates Euclidean distance between two fixed-point coordinates."
    parameters:
      - name: V1
        type: TFixedPoint
        description: "First point."
      - name: V2
        type: TFixedPoint
        description: "Second point."
    returns:
      - type: TFixed
        description: "The Euclidean distance between V1 and V2."
  - signature: "function Distance(const V1, V2: TPoint): TFloat; overload;"
    summary: "Calculates Euclidean distance between two integer coordinates."
    parameters:
      - name: V1
        type: TPoint
        description: "First point."
      - name: V2
        type: TPoint
        description: "Second point."

    returns:
      - type: TFloat
        description: "The Euclidean distance between V1 and V2."
---

## Description

`Distance` computes the Euclidean distance between two 2D points $V1$ and $V2$, defined mathematically as:

$$\text{Distance}(V1, V2) = \sqrt{(V2.X - V1.X)^2 + (V2.Y - V1.Y)^2}$$

If you only need to compare relative distances without needing the exact Euclidean distance, consider using [[SqrDistance]] instead to eliminate the computational cost of the square root operation.

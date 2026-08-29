---
layout: doc
docType: api
unit: GR32_Geometry
entity: SamePoint
kind: Function
summary: "Tests whether two points are coincident within a specified squared distance tolerance."
overloads:
  - signature: "function SamePoint(const A, B: TFloatPoint; SqrDist: Double): Boolean; overload;"
    summary: "Tests if floating-point points A and B are closer than SqrDist tolerance."
    parameters:
      - name: A, B
        type: TFloatPoint
        description: "Points to compare."
      - name: SqrDist
        type: Double
        description: "Squared distance threshold."
  - signature: "function SamePoint(const A, B: TFixedPoint; SqrDist: TFixed): Boolean; overload;"
    summary: "Tests if fixed-point points A and B are closer than SqrDist tolerance."
    parameters:
      - name: A, B
        type: TFixedPoint
        description: "Points to compare."
      - name: SqrDist
        type: TFixed
        description: "Squared distance threshold."
  - signature: "function SamePoint(const A, B: TPoint; SqrDist: integer): Boolean; overload;"
    summary: "Tests if integer points A and B are closer than SqrDist tolerance."
    parameters:
      - name: A, B
        type: TPoint
        description: "Points to compare."
      - name: SqrDist
        type: Integer
        description: "Squared distance threshold."
---

## Description

`SamePoint` evaluates whether points $A$ and $B$ are effectively at the same location by testing whether their squared Euclidean distance is strictly less than `SqrDist`:

$$\text{SqrDistance}(A, B) < \text{SqrDist}$$

Using squared distance avoids square root calculations.

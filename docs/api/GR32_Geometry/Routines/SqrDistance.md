---
layout: doc
docType: api
unit: GR32_Geometry
entity: SqrDistance
kind: Function
summary: "Calculates the squared Euclidean distance between two points."
overloads:
  - signature: "function SqrDistance(const V1, V2: TFloatPoint): TFloat; overload;"
    summary: "Calculates squared Euclidean distance between two floating-point points."
    parameters:
      - name: V1
        type: TFloatPoint
        description: "First point."
      - name: V2
        type: TFloatPoint
        description: "Second point."
    returns:
      - type: TFloat
        description: "The squared Euclidean distance between V1 and V2."
  - signature: "function SqrDistance(const V1, V2: TFixedPoint): TFixed; overload;"
    summary: "Calculates squared Euclidean distance between two fixed-point points."
    parameters:
      - name: V1
        type: TFixedPoint
        description: "First point."
      - name: V2
        type: TFixedPoint
        description: "Second point."
    returns:
      - type: TFixed
        description: "The squared Euclidean distance between V1 and V2."
  - signature: "function SqrDistance(const V1, V2: TPoint): Integer; overload;"
    summary: "Calculates squared Euclidean distance between two integer points."
    parameters:
      - name: V1
        type: TPoint
        description: "First point."
      - name: V2
        type: TPoint
        description: "Second point."

    returns:
      - type: Integer
        description: "The squared Euclidean distance between V1 and V2."
---

## Description

`SqrDistance` calculates the squared Euclidean distance between points $V1$ and $V2$, defined as:

$$\text{SqrDistance}(V1, V2) = (V2.X - V1.X)^2 + (V2.Y - V1.Y)^2$$

`SqrDistance` avoids performing a square root operation, making it significantly faster than [[Distance]] for point distance comparisons, proximity checks, and bounding-sphere tests.

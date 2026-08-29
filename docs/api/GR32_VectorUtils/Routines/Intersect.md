---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Intersect
kind: Function
summary: "Computes the 2D intersection point of two line segments."
overloads:
  - signature: "function Intersect(const A1, A2, B1, B2: TFloatPoint; out P: TFloatPoint): Boolean; overload;"
    summary: "Computes the float intersection point P of segment (A1, A2) and segment (B1, B2)."
    parameters:
      - name: A1, A2
        type: TFloatPoint
        description: "First line segment endpoints."
      - name: B1, B2
        type: TFloatPoint
        description: "Second line segment endpoints."
      - name: P
        type: TFloatPoint
        description: "Output intersection point."

  - signature: "function Intersect(const A1, A2, B1, B2: TFixedPoint; out P: TFixedPoint): Boolean; overload;"
    summary: "Computes the fixed-point intersection point P of segment (A1, A2) and segment (B1, B2)."
    parameters:
      - name: A1, A2
        type: TFixedPoint
        description: "First line segment endpoints."
      - name: B1, B2
        type: TFixedPoint
        description: "Second line segment endpoints."
      - name: P
        type: TFixedPoint
        description: "Output intersection point."
---

## Description

`Intersect` calculates the 2D intersection point `P` of line segment `A1-A2` and line segment `B1-B2`. Returns `True` if the segments intersect.

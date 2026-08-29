---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: PolygonBounds
kind: Function
summary: "Computes the minimal axis-aligned bounding rectangle of a single polygon contour."
overloads:
  - signature: "function PolygonBounds(const Points: TArrayOfFloatPoint): TFloatRect; overload;"
    summary: "Computes the TFloatRect bounds of a floating-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."

  - signature: "function PolygonBounds(const Points: TArrayOfFixedPoint): TFixedRect; overload;"
    summary: "Computes the TFixedRect bounds of a fixed-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
---

## Description

`PolygonBounds` calculates the minimal bounding rectangle enclosing all vertices in `Points`.

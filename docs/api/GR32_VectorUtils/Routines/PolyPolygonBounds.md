---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: PolyPolygonBounds
kind: Function
summary: "Computes the minimal axis-aligned bounding rectangle enclosing a multi-contour polygon."
overloads:
  - signature: "function PolyPolygonBounds(const Points: TArrayOfArrayOfFloatPoint): TFloatRect; overload;"
    summary: "Computes the TFloatRect bounds enclosing all floating-point polygon contours."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours."

    returns:
      - type: TFloatRect
        description: "The [[TFloatRect]] bounding rectangle."
  - signature: "function PolyPolygonBounds(const Points: TArrayOfArrayOfFixedPoint): TFixedRect; overload;"
    summary: "Computes the TFixedRect bounds enclosing all fixed-point polygon contours."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."

    returns:
      - type: TFixedRect
        description: "The [[TFixedRect]] bounding rectangle."
---

## Description

`PolyPolygonBounds` calculates the union bounding rectangle enclosing all contours in `Points`.

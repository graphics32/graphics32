---
layout: doc
docType: api
unit: GR32_Geometry
entity: PointInPolygon
kind: Function
summary: "Determines whether a point lies inside a closed polygon using ray casting algorithm."
overloads:
  - signature: "function PointInPolygon(const Pt: TFloatPoint; const Pts: TArrayOfFloatPoint): Boolean; overload;"
    summary: "Tests if a floating-point coordinate lies inside a floating-point polygon."
    parameters:
      - name: Pt
        type: TFloatPoint
        description: "Test point."
      - name: Pts
        type: TArrayOfFloatPoint
        description: "Vertices of closed polygon."
  - signature: "function PointInPolygon(const Pt: TFixedPoint; const Pts: array of TFixedPoint): Boolean; overload;"
    summary: "Tests if a fixed-point coordinate lies inside a fixed-point polygon."
    parameters:
      - name: Pt
        type: TFixedPoint
        description: "Test point."
      - name: Pts
        type: array of TFixedPoint
        description: "Vertices of closed polygon."
---

## Description

`PointInPolygon` implements the ray-casting algorithm (crossing number algorithm) to determine whether point `Pt` is located inside the closed polygon defined by vertex array `Pts`.

Returns `True` if `Pt` lies inside the polygon, and `False` if it lies outside.

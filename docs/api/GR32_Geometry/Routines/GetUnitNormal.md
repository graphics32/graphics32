---
layout: doc
docType: api
unit: GR32_Geometry
entity: GetUnitNormal
kind: Function
summary: "Calculates a normalized unit perpendicular (normal) vector for line segment Pt1 -> Pt2."
overloads:
  - signature: "function GetUnitNormal(const Pt1, Pt2: TFloatPoint): TFloatPoint; overload;"
    summary: "Returns the perpendicular unit normal vector for line segment Pt1 -> Pt2."
    parameters:
      - name: Pt1
        type: TFloatPoint
        description: "Start point of line segment."
      - name: Pt2
        type: TFloatPoint
        description: "End point of line segment."
  - signature: "procedure GetUnitNormal(const Pt1, Pt2: TFloatPoint; out Result: TFloatPoint); overload;"
    summary: "Calculates the perpendicular unit normal vector using an out parameter."
    parameters:
      - name: Pt1
        type: TFloatPoint
        description: "Start point of line segment."
      - name: Pt2
        type: TFloatPoint
        description: "End point of line segment."
      - name: Result
        type: TFloatPoint
        description: "Output variable receiving unit normal vector."
  - signature: "function GetUnitNormal(const Pt1, Pt2: TFixedPoint): TFloatPoint; overload;"
    summary: "Returns perpendicular unit normal vector for fixed-point line segment Pt1 -> Pt2."
    parameters:
      - name: Pt1
        type: TFixedPoint
        description: "Start point of line segment."
      - name: Pt2
        type: TFixedPoint
        description: "End point of line segment."
---

## Description

`GetUnitNormal` computes a normalized 2D vector (length 1.0) that is perpendicular to the vector pointing from `Pt1` to `Pt2`.

If `Pt1` and `Pt2` are coincident (zero length segment), the function returns `(0, 0)`.

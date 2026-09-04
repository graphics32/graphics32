---
layout: doc
docType: api
unit: GR32_Geometry
entity: GetUnitVector
kind: Function
summary: "Calculates a normalized unit direction vector from Pt1 to Pt2."
overloads:
  - signature: "function GetUnitVector(const Pt1, Pt2: TFloatPoint): TFloatPoint; overload;"
    summary: "Returns unit vector pointing from floating-point Pt1 to Pt2."
    parameters:
      - name: Pt1
        type: TFloatPoint
        description: "Start point."
      - name: Pt2
        type: TFloatPoint
        description: "End point."
    returns:
      - type: TFloatPoint
        description: "A [[TFloatPoint]] unit vector pointing in the direction from Pt1 to Pt2."
  - signature: "function GetUnitVector(const Pt1, Pt2: TFixedPoint): TFloatPoint; overload;"
    summary: "Returns unit vector pointing from fixed-point Pt1 to Pt2."
    parameters:
      - name: Pt1
        type: TFixedPoint
        description: "Start point."
      - name: Pt2
        type: TFixedPoint
        description: "End point."

    returns:
      - type: TFloatPoint
        description: "A [[TFloatPoint]] unit vector pointing in the direction from Pt1 to Pt2."
---

## Description

`GetUnitVector` calculates the unit direction vector (a vector of length 1.0) pointing from origin `Pt1` to destination `Pt2`.

If `Pt1` and `Pt2` are coincident, the routine returns `(0, 0)`.

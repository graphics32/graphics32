---
layout: doc
docType: api
unit: GR32_Geometry
entity: GetAngleOfPt2FromPt1
kind: Function
summary: "Calculates the polar angle in radians from Pt1 to Pt2."
overloads:
  - signature: "function GetAngleOfPt2FromPt1(const Pt1, Pt2: TFloatPoint): Single; overload;"
    summary: "Calculates angle in radians from floating-point Pt1 to Pt2."
    parameters:
      - name: Pt1
        type: TFloatPoint
        description: "Origin reference point."
      - name: Pt2
        type: TFloatPoint
        description: "Target point."
    returns:
      - type: Single
        description: "The directional angle in radians from Pt1 to Pt2 in range [0..2pi)."
  - signature: "function GetAngleOfPt2FromPt1(Pt1, Pt2: TFixedPoint): Single; overload;"
    summary: "Calculates angle in radians from fixed-point Pt1 to Pt2."
    parameters:
      - name: Pt1
        type: TFixedPoint
        description: "Origin reference point."
      - name: Pt2
        type: TFixedPoint
        description: "Target point."

    returns:
      - type: Single
        description: "The directional angle in radians from Pt1 to Pt2 in range [0..2pi)."
---

## Description

`GetAngleOfPt2FromPt1` calculates the directional angle in radians from point `Pt1` to point `Pt2`. The returned angle is normalized within the range $[0..2\pi)$.

Vertical alignment edge cases ($X_1 = X_2$) return `CRad270` ($3\pi/2$) when $Y_2 > Y_1$ and `CRad90` ($\pi/2$) when $Y_2 < Y_1$.

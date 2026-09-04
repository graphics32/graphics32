---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ReversePolygon
kind: Function
summary: "Reverses the vertex ordering / winding direction of a polygon."
overloads:
  - signature: "function ReversePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint; overload;"
    summary: "Reverses the order of vertices in a floating-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function ReversePolygon(const Points: TArrayOfFixedPoint): TArrayOfFixedPoint; overload;"
    summary: "Reverses the order of vertices in a fixed-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`ReversePolygon` inverts the vertex sequence of `Points` to flip winding direction from clockwise to counter-clockwise or vice versa.

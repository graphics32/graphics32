---
layout: doc
docType: api
unit: GR32_Geometry
entity: PerpendicularDistance
kind: Function
summary: "Calculates the perpendicular (shortest) distance from a point P to infinite line P1-P2."
overloads:
  - signature: "function PerpendicularDistance(const P, P1, P2: TFloatPoint): TFloat; overload;"
    summary: "Calculates perpendicular distance from floating-point P to line P1-P2."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Test point."
      - name: P1, P2
        type: TFloatPoint
        description: "Points defining infinite line."
    returns:
      - type: TFloat
        description: "The perpendicular distance from point P to line P1-P2."
  - signature: "function PerpendicularDistance(const P, P1, P2: TFixedPoint): TFixed; overload;"
    summary: "Calculates perpendicular distance from fixed-point P to line P1-P2."
    parameters:
      - name: P
        type: TFixedPoint
        description: "Test point."
      - name: P1, P2
        type: TFixedPoint
        description: "Points defining infinite line."
    returns:
      - type: TFixed
        description: "The perpendicular distance from point P to line P1-P2."
  - signature: "function PerpendicularDistance(const P, P1, P2: TPoint): TFloat; overload;"
    summary: "Calculates perpendicular distance from integer P to line P1-P2."
    parameters:
      - name: P
        type: TPoint
        description: "Test point."
      - name: P1, P2
        type: TPoint
        description: "Points defining infinite line."

    returns:
      - type: TFloat
        description: "The perpendicular distance from point P to line P1-P2."
---

## Description

`PerpendicularDistance` calculates the minimum distance (shortest distance perpendicular to the line) from point $P$ to the infinite line extending through $P1$ and $P2$.

---
layout: doc
docType: api
unit: GR32_Geometry
entity: Shorten
kind: Function
summary: "Trims/shortens the endpoints of a polyline or point array by a specified distance."
overloads:
  - signature: "function Shorten(const Pts: TArrayOfFloatPoint; Delta: TFloat; LinePos: TLinePos): TArrayOfFloatPoint; overload;"
    summary: "Shortens floating-point polyline array endpoints by Delta distance."
    parameters:
      - name: Pts
        type: TArrayOfFloatPoint
        description: "Input array of points representing polyline."
      - name: Delta
        type: TFloat
        description: "Distance to shorten."
      - name: LinePos
        type: TLinePos
        description: "Endpoint selection (`lpStart`, `lpEnd`, `lpBoth`, `lpNeither`)."
    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] containing the shortened polyline points."
  - signature: "function Shorten(const Pts: TArrayOfFixedPoint; Delta: TFloat; LinePos: TLinePos): TArrayOfFixedPoint; overload;"
    summary: "Shortens fixed-point polyline array endpoints by Delta distance."
    parameters:
      - name: Pts
        type: TArrayOfFixedPoint
        description: "Input array of points representing polyline."
      - name: Delta
        type: TFloat
        description: "Distance to shorten."
      - name: LinePos
        type: TLinePos
        description: "Endpoint selection (`lpStart`, `lpEnd`, `lpBoth`, `lpNeither`)."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] containing the shortened polyline points."
---

## Description

`Shorten` trims polyline endpoint(s) by moving the start point, end point, or both inwards along the line direction by `Delta` units. If intermediate segments are shorter than `Delta`, inner vertices are automatically discarded until the full distance is trimmed.

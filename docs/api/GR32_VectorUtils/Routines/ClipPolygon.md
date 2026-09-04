---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ClipPolygon
kind: Function
summary: "Clips a polygon to a bounding rectangle using Sutherland-Hodgman clipping."
overloads:
  - signature: "function ClipPolygon(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect): TArrayOfFloatPoint; overload;"
    summary: "Clips a floating-point polygon to a TFloatRect."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: ClipRect
        type: TFloatRect
        description: "Clipping rectangle."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function ClipPolygon(const Points: TArrayOfFixedPoint; const ClipRect: TFixedRect): TArrayOfFixedPoint; overload;"
    summary: "Clips a fixed-point polygon to a TFixedRect."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Polygon vertices."
      - name: ClipRect
        type: TFixedRect
        description: "Clipping rectangle."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`ClipPolygon` clips input polygon contours against a 2D bounding rectangle using Sutherland-Hodgman polygon clipping.

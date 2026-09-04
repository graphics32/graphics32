---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: TranslatePolyPolygon
kind: Function
summary: "Translates multi-contour polygon coordinates."
overloads:
  - signature: "function TranslatePolyPolygon(const Points: TArrayOfArrayOfFloatPoint; OffsetX, OffsetY: TFloat): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Translates multi-contour floating-point polygons by (OffsetX, OffsetY)."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours."
      - name: OffsetX, OffsetY
        type: TFloat
        description: "Offsets."

    returns:
      - type: TArrayOfArrayOfFloatPoint
        description: "A [[TArrayOfArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function TranslatePolyPolygon(const Points: TArrayOfArrayOfFixedPoint; OffsetX, OffsetY: TFixed): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Translates multi-contour fixed-point polygons by (OffsetX, OffsetY)."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
      - name: OffsetX, OffsetY
        type: TFixed
        description: "Offsets."

    returns:
      - type: TArrayOfArrayOfFixedPoint
        description: "A [[TArrayOfArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`TranslatePolyPolygon` offsets all contours in `Points` by `(OffsetX, OffsetY)`.

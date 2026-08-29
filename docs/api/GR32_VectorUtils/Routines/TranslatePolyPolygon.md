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

  - signature: "function TranslatePolyPolygon(const Points: TArrayOfArrayOfFixedPoint; OffsetX, OffsetY: TFixed): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Translates multi-contour fixed-point polygons by (OffsetX, OffsetY)."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
      - name: OffsetX, OffsetY
        type: TFixed
        description: "Offsets."
---

## Description

`TranslatePolyPolygon` offsets all contours in `Points` by `(OffsetX, OffsetY)`.

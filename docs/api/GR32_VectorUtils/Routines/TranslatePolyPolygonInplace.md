---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: TranslatePolyPolygonInplace
kind: Function
summary: "Translates multi-contour polygon coordinates in-place."
overloads:
  - signature: "procedure TranslatePolyPolygonInplace(const Points: TArrayOfArrayOfFloatPoint; OffsetX, OffsetY: TFloat); overload;"
    summary: "Translates multi-contour floating-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours (modified in-place)."
      - name: OffsetX, OffsetY
        type: TFloat
        description: "Offsets."

  - signature: "procedure TranslatePolyPolygonInplace(const Points: TArrayOfArrayOfFixedPoint; OffsetX, OffsetY: TFixed); overload;"
    summary: "Translates multi-contour fixed-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours (modified in-place)."
      - name: OffsetX, OffsetY
        type: TFixed
        description: "Offsets."
---

## Description

`TranslatePolyPolygonInplace` offsets all contours in `Points` by `(OffsetX, OffsetY)` in-place.

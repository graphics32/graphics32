---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ScalePolyPolygonInplace
kind: Function
summary: "Scales multi-contour polygon coordinates in-place."
overloads:
  - signature: "procedure ScalePolyPolygonInplace(const Points: TArrayOfArrayOfFloatPoint; ScaleX, ScaleY: TFloat); overload;"
    summary: "Scales multi-contour floating-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours (modified in-place)."
      - name: ScaleX, ScaleY
        type: TFloat
        description: "Scale multipliers."

  - signature: "procedure ScalePolyPolygonInplace(const Points: TArrayOfArrayOfFixedPoint; ScaleX, ScaleY: TFixed); overload;"
    summary: "Scales multi-contour fixed-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours (modified in-place)."
      - name: ScaleX, ScaleY
        type: TFixed
        description: "Scale multipliers."

---

## Description

`ScalePolyPolygonInplace` multiplies vertex coordinates across all contours in `Points` by `ScaleX` and `ScaleY` in-place.

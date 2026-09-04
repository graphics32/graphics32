---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: RoundRect
kind: Function
declaration: "function RoundRect(const R: TFloatRect; const Radius: TFloat): TArrayOfFloatPoint;"
summary: "Generates rounded rectangle polygon contours."
parameters:
  - name: R
    type: TFloatRect
    description: "Bounding rectangle."
  - name: Radius
    type: TFloat
    description: "Corner rounding radius in pixels."
returns:
  - type: TArrayOfFloatPoint
    description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
---

## Description

`RoundRect` constructs a closed polygon contour representing rectangle `R` with rounded corners of radius `Radius`.

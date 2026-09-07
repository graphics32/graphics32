---
layout: doc
docType: api
unit: GR32_Layers
parent: TPositionedLayer
entity: TPositionedLayer.GetAdjustedRect
kind: Method
scope: Public
declaration: "function GetAdjustedRect(const R: TFloatRect): TFloatRect; virtual;"
summary: "Transforms a floating-point rectangle from local bitmap coordinates to viewport coordinates."
parameters:
  - name: R
    type: TFloatRect
    description: "Rectangle in local coordinates."
returns:
  - type: TFloatRect
    description: "Transformed rectangle in viewport coordinates."
---

## Description

`GetAdjustedRect` converts rectangle `R` to viewport coordinates taking `Scaled` into account.

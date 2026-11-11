---
layout: doc
docType: api
unit: GR32_Math
entity: FixedSqrtHP
kind: Function
declaration: "function FixedSqrtHP(Value: TFixed): TFixed;"
summary: "Calculates the square root of a fixed-point number with high precision (16-bit fractional accuracy)."
parameters:
  - name: Value
    type: TFixed
    description: "The non-negative fixed-point input value."
returns:
  - type: TFixed
    description: "The square root in 16.16 fixed-point format."
seealso:
  - "[[FixedSqrtLP]]"
  - "[[FixedSqr]]"
---

## Description

`FixedSqrtHP` computes the square root of a 16.16 fixed-point number with full 16-bit fractional precision.

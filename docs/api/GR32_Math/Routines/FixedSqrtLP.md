---
layout: doc
docType: api
unit: GR32_Math
entity: FixedSqrtLP
kind: Function
declaration: "function FixedSqrtLP(Value: TFixed): TFixed;"
summary: "Calculates the square root of a fixed-point number with low precision (8-bit fractional accuracy)."
parameters:
  - name: Value
    type: TFixed
    description: "The non-negative fixed-point input value."
returns:
  - type: TFixed
    description: "The square root in 16.16 fixed-point format."
seealso:
  - "[[FixedSqrtHP]]"
  - "[[FixedSqr]]"
---

## Description

`FixedSqrtLP` computes the square root of a 16.16 fixed-point number using a fast bitwise integer algorithm optimized for 8-bit fractional precision.

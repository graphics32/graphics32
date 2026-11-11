---
layout: doc
docType: api
unit: GR32_Math
entity: FixedSqr
kind: Function
declaration: "function FixedSqr(Value: TFixed): TFixed;"
summary: "Calculates the square of a fixed-point number."
parameters:
  - name: Value
    type: TFixed
    description: "The fixed-point value."
returns:
  - type: TFixed
    description: "The squared result in 16.16 fixed-point format."
seealso:
  - "[[FixedMul]]"
  - "[[FixedSqrtLP]]"
  - "[[FixedSqrtHP]]"
---

## Description

`FixedSqr` computes the square (`Value * Value`) of a 16.16 fixed-point number.

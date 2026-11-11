---
layout: doc
docType: api
unit: GR32_Math
entity: FixedDiv
kind: Function
declaration: "function FixedDiv(A, B: TFixed): TFixed;"
summary: "Divides one fixed-point number by another."
parameters:
  - name: A
    type: TFixed
    description: "Dividend in 16.16 fixed-point format."
  - name: B
    type: TFixed
    description: "Divisor in 16.16 fixed-point format."
returns:
  - type: TFixed
    description: "The quotient in 16.16 fixed-point format."
seealso:
  - "[[FixedMul]]"
  - "[[OneOver]]"
---

## Description

`FixedDiv` divides 16.16 fixed-point number `A` by `B` and returns the result in 16.16 fixed-point format.

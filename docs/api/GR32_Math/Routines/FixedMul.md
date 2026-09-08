---
layout: doc
docType: api
unit: GR32_Math
entity: FixedMul
kind: Function
declaration: "function FixedMul(A, B: TFixed): TFixed;"
summary: "Multiplies two fixed-point numbers."
parameters:
  - name: A
    type: TFixed
    description: "First fixed-point operand."
  - name: B
    type: TFixed
    description: "Second fixed-point operand."
returns:
  - type: TFixed
    description: "The product in 16.16 fixed-point format."
seealso:
  - "[[FixedDiv]]"
  - "[[FixedSqr]]"
---

## Description

`FixedMul` performs multiplication of two 16.16 fixed-point numbers `A` and `B` and scales the result back to 16.16 fixed-point format.

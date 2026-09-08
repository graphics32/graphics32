---
layout: doc
docType: api
unit: GR32_Math
entity: FixedCeil
kind: Function
declaration: "function FixedCeil(A: TFixed): Integer;"
summary: "Returns the smallest integer greater than or equal to a fixed-point value."
parameters:
  - name: A
    type: TFixed
    description: "Fixed-point value in 16.16 format."
returns:
  - type: Integer
    description: "The ceiling integer result."
seealso:
  - "[[FixedFloor]]"
  - "[[FixedRound]]"
---

## Description

`FixedCeil` computes the ceiling of a 16.16 fixed-point number `A`, returning the smallest integer greater than or equal to `A`.

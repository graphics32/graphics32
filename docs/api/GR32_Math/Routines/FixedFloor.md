---
layout: doc
docType: api
unit: GR32_Math
entity: FixedFloor
kind: Function
declaration: "function FixedFloor(A: TFixed): Integer;"
summary: "Returns the greatest integer less than or equal to a fixed-point value."
parameters:
  - name: A
    type: TFixed
    description: "Fixed-point value in 16.16 format."
returns:
  - type: Integer
    description: "The floored integer result."
seealso:
  - "[[FixedCeil]]"
  - "[[FixedRound]]"
---

## Description

`FixedFloor` computes the floor of a 16.16 fixed-point number `A`, returning the largest integer less than or equal to `A`.

In 16.16 fixed-point format, this operation is equivalent to a arithmetic right shift by 16 bits.

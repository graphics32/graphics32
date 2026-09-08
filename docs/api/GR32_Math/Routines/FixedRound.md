---
layout: doc
docType: api
unit: GR32_Math
entity: FixedRound
kind: Function
declaration: "function FixedRound(A: TFixed): Integer;"
summary: "Rounds a fixed-point value to the nearest integer."
parameters:
  - name: A
    type: TFixed
    description: "Fixed-point value in 16.16 format."
returns:
  - type: Integer
    description: "The rounded integer result."
seealso:
  - "[[FixedFloor]]"
  - "[[FixedCeil]]"
---

## Description

`FixedRound` rounds a 16.16 fixed-point value `A` to the nearest integer.

---
layout: doc
docType: api
unit: GR32_Math
entity: FastSqrtBab2
kind: Function
declaration: "function FastSqrtBab2(const Value: TFloat): TFloat;"
summary: "Computes a fast square root approximation refined with two Babylonian iteration steps."
parameters:
  - name: Value
    type: TFloat
    description: "Input floating-point value."
returns:
  - type: TFloat
    description: "Approximated square root."
seealso:
  - "[[FastSqrt]]"
  - "[[FastSqrtBab1]]"
---

## Description

`FastSqrtBab2` computes a fast square root approximation using bit manipulation refined with two Babylonian method iteration steps for higher precision than `FastSqrtBab1`.

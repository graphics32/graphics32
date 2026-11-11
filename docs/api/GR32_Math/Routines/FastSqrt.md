---
layout: doc
docType: api
unit: GR32_Math
entity: FastSqrt
kind: Function
declaration: "function FastSqrt(const Value: TFloat): TFloat;"
summary: "Computes a fast floating-point square root approximation."
parameters:
  - name: Value
    type: TFloat
    description: "Input floating-point value."
returns:
  - type: TFloat
    description: "Approximated square root."
seealso:
  - "[[FastSqrtBab1]]"
  - "[[FastSqrtBab2]]"
  - "[[FastInvSqrt]]"
---

## Description

`FastSqrt` provides a high-speed approximation of the square root function for positive floating-point numbers using SIMD or bit manipulation techniques.

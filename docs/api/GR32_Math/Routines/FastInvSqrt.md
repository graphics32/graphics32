---
layout: doc
docType: api
unit: GR32_Math
entity: FastInvSqrt
kind: Function
declaration: "function FastInvSqrt(const Value: TFloat): TFloat;"
summary: "Computes a fast inverse square root (1 / sqrt(Value)) approximation."
parameters:
  - name: Value
    type: TFloat
    description: "Input positive floating-point value."
returns:
  - type: TFloat
    description: "Approximated inverse square root value."
seealso:
  - "[[FastSqrt]]"
---

## Description

`FastInvSqrt` calculates a fast approximation of $1 / \sqrt{\text{Value}}$ using hardware RSQRT instructions or fast bit-level magic constant algorithms.

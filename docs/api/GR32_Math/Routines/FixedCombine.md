---
layout: doc
docType: api
unit: GR32_Math
entity: FixedCombine
kind: Function
declaration: "function FixedCombine(W, X, Y: TFixed): TFixed;"
summary: "Linearly interpolates between two fixed-point values using a fixed-point weight."
parameters:
  - name: W
    type: TFixed
    description: "Interpolation weight in [0..FixedOne] range."
  - name: X
    type: TFixed
    description: "First fixed-point value (weight W)."
  - name: Y
    type: TFixed
    description: "Second fixed-point value (weight 1 - W)."
returns:
  - type: TFixed
    description: "The interpolated fixed-point result."
seealso:
  - "[[FixedMul]]"
---

## Description

`FixedCombine` computes the linear combination $Y + (X - Y) \cdot W / 65536$ for fixed-point values $X$ and $Y$ with weight $W$.

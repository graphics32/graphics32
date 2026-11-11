---
layout: doc
docType: api
unit: GR32_Math
entity: MulDiv
kind: Function
declaration: "function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;"
summary: "Multiplies two integers and divides the 64-bit product by a third integer with nearest-integer rounding."
parameters:
  - name: Multiplicand
    type: Integer
    description: "Multiplicand value."
  - name: Multiplier
    type: Integer
    description: "Multiplier value."
  - name: Divisor
    type: Integer
    description: "Divisor value."
returns:
  - type: Integer
    description: "The rounded result of (Multiplicand * Multiplier) / Divisor."
seealso:
  - "[[DivMod]]"
---

## Description

`MulDiv` multiplies `Multiplicand` by `Multiplier` using 64-bit intermediate precision to prevent overflow before dividing by `Divisor`. The result is rounded to the nearest integer.

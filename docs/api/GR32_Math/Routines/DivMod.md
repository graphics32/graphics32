---
layout: doc
docType: api
unit: GR32_Math
entity: DivMod
kind: Function
declaration: "function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;"
summary: "Simultaneously calculates integer quotient and remainder."
parameters:
  - name: Dividend
    type: Integer
    description: "The dividend integer."
  - name: Divisor
    type: Integer
    description: "The divisor integer."
  - name: Remainder
    type: Integer
    description: "Output variable receiving the division remainder."
returns:
  - type: Integer
    description: "The integer quotient (Dividend div Divisor)."
seealso:
  - "[[MulDiv]]"
---

## Description

`DivMod` performs integer division, returning the quotient as the function result and storing the remainder in the `Remainder` variable in a single operation.

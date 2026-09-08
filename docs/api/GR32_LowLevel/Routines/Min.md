---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Min
kind: Function
declaration: "function Min(const A, B, C: Integer): Integer; overload;"
summary: "Returns the minimum value among three integer values."
parameters:
  - name: A, B, C
    type: Integer
    description: "Three integer values to compare."
returns:
  - type: Integer
    description: "The smallest of the three input integer values."
seealso:
  - "[[Max]]"
  - "[[Clamp]]"
---

## Description

`Min` compares three integer values `A`, `B`, and `C` and returns the minimum value among them.

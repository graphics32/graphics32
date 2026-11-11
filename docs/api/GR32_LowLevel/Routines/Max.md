---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Max
kind: Function
declaration: "function Max(const A, B, C: Integer): Integer; overload;"
summary: "Returns the maximum value among three integer values."
parameters:
  - name: A, B, C
    type: Integer
    description: "Three integer values to compare."
returns:
  - type: Integer
    description: "The largest of the three input integer values."
seealso:
  - "[[Min]]"
  - "[[Clamp]]"
---

## Description

`Max` compares three integer values `A`, `B`, and `C` and returns the maximum value among them.

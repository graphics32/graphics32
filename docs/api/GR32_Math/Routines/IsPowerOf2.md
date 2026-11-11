---
layout: doc
docType: api
unit: GR32_Math
entity: IsPowerOf2
kind: Function
declaration: "function IsPowerOf2(Value: Integer): Boolean;"
summary: "Tests if a non-negative integer is a power of two."
parameters:
  - name: Value
    type: Integer
    description: "Value to test (must be >= 0)."
returns:
  - type: Boolean
    description: "True if Value is a power of two (1, 2, 4, 8, 16, etc.), False otherwise."
seealso:
  - "[[PrevPowerOf2]]"
  - "[[NextPowerOf2]]"
---

## Description

`IsPowerOf2` determines whether `Value` is a power of two (such as $1, 2, 4, 8, 16, \dots$) using bitwise operations.

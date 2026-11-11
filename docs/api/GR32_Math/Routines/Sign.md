---
layout: doc
docType: api
unit: GR32_Math
entity: Sign
kind: Function
declaration: "function Sign(Value: Integer): Integer;"
summary: "Returns the sign of an integer (-1, 0, or 1)."
parameters:
  - name: Value
    type: Integer
    description: "Input integer."
returns:
  - type: Integer
    description: "-1 if Value < 0, 0 if Value = 0, +1 if Value > 0."
---

## Description

`Sign` returns `-1` if `Value` is negative, `0` if `Value` is zero, and `1` if `Value` is positive.

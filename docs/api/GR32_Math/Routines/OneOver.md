---
layout: doc
docType: api
unit: GR32_Math
entity: OneOver
kind: Function
declaration: "function OneOver(Value: TFixed): TFixed;"
summary: "Calculates the reciprocal (1 / Value) of a fixed-point number."
parameters:
  - name: Value
    type: TFixed
    description: "The fixed-point value."
returns:
  - type: TFixed
    description: "The reciprocal in 16.16 fixed-point format."
seealso:
  - "[[FixedDiv]]"
---

## Description

`OneOver` computes `1.0 / Value` for 16.16 fixed-point numbers. It is equivalent to calling `FixedDiv(FixedOne, Value)`.

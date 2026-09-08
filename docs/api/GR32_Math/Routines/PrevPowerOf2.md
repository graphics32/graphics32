---
layout: doc
docType: api
unit: GR32_Math
entity: PrevPowerOf2
kind: Function
declaration: "function PrevPowerOf2(Value: Integer): Integer;"
summary: "Rounds an integer down to the previous power of two."
parameters:
  - name: Value
    type: Integer
    description: "Input integer value."
returns:
  - type: Integer
    description: "The highest power of two less than or equal to Value."
seealso:
  - "[[NextPowerOf2]]"
  - "[[IsPowerOf2]]"
---

## Description

`PrevPowerOf2` rounds `Value` down to the previous power of two (e.g. $5 \to 4$, $7 \to 4$, $8 \to 4$, $9 \to 8$).

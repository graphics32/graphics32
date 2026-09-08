---
layout: doc
docType: api
unit: GR32_Math
entity: NextPowerOf2
kind: Function
declaration: "function NextPowerOf2(Value: Integer): Integer;"
summary: "Rounds an integer up to the next power of two."
parameters:
  - name: Value
    type: Integer
    description: "Input integer value."
returns:
  - type: Integer
    description: "The smallest power of two greater than or equal to Value."
seealso:
  - "[[PrevPowerOf2]]"
  - "[[IsPowerOf2]]"
---

## Description

`NextPowerOf2` rounds `Value` up to the next power of two (e.g. $5 \to 8$, $7 \to 8$, $8 \to 16$, $15 \to 16$).

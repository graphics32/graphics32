---
layout: doc
docType: api
unit: GR32_LowLevel
entity: SwapConstrain
kind: Function
declaration: "function SwapConstrain(const Value: Integer; Constrain1, Constrain2: Integer): Integer;"
summary: "Constrains an integer value to a range defined by two bounds regardless of their ordering."
parameters:
  - name: Value
    type: Integer
    description: "Integer value to constrain."
  - name: Constrain1
    type: Integer
    description: "First boundary constraint."
  - name: Constrain2
    type: Integer
    description: "Second boundary constraint."
returns:
  - type: Integer
    description: "Value constrained to [min(Constrain1, Constrain2) .. max(Constrain1, Constrain2)]."
seealso:
  - "[[Constrain]]"
  - "[[Clamp]]"
---

## Description

`SwapConstrain` restricts `Value` to the interval bounded by `Constrain1` and `Constrain2`. Unlike [[Constrain]], `Constrain1` does not need to be less than or equal to `Constrain2`; `SwapConstrain` automatically determines the minimum and maximum of the two bounds.

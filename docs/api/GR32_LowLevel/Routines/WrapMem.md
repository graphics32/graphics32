---
layout: doc
docType: api
unit: GR32_LowLevel
entity: WrapMem
kind: Procedure
declaration: "procedure WrapMem(var Value: Single; Max: Cardinal);"
summary: "Wraps a floating-point variable in-place to range [0..Max)."
parameters:
  - name: Value
    type: Single
    description: "Floating-point single variable to wrap in-place."
  - name: Max
    type: Cardinal
    description: "Integer maximum range limit."
seealso:
  - "[[Wrap]]"
---

## Description

`WrapMem` wraps the floating-point variable `Value` in place so that its resulting coordinate lies within the half-open interval `[0..Max)`.

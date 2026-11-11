---
layout: doc
docType: api
unit: GR32_Blend
entity: TCombineMem
kind: Type
declaration: "type TCombineMem = procedure(X: TColor32; var Y: TColor32; W: Cardinal);"
summary: "Procedural delegate for linear interpolation (Lerp) into a memory-referenced color variable."
parameters:
  - name: X
    type: TColor32
    description: "First color value."
  - name: Y
    type: TColor32
    description: "Reference to the second color variable, updated in-place with the combined result."
  - name: W
    type: Cardinal
    description: "Interpolation weight (0..255)."
seealso:
  - "[[CombineMem]]"
  - "[[CombineReg]]"
---

## Description

`TCombineMem` defines the signature for memory-in-place linear combination routines, interpolating color `X` with destination color variable `Y` using weight `W` ($0 \dots 255$).

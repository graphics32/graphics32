---
layout: doc
docType: api
unit: GR32_Blend
entity: TCombineLine
kind: Type
declaration: "type TCombineLine = procedure(Src, Dst: PColor32; Count: Integer; W: Cardinal);"
summary: "Procedural delegate for linear interpolation across scanline buffers with a constant weight."
parameters:
  - name: Src
    type: PColor32
    description: "Pointer to the first source color in memory."
  - name: Dst
    type: PColor32
    description: "Pointer to the first destination color in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous color elements to combine."
  - name: W
    type: Cardinal
    description: "Interpolation weight (0..255)."
seealso:
  - "[[CombineLine]]"
  - "[[CombineReg]]"
---

## Description

`TCombineLine` defines the signature for scanline linear combination routines, interpolating `Count` pairs of colors between `Src` and `Dst` buffers using weight `W` ($0 \dots 255$).

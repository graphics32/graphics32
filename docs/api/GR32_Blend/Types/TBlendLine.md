---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendLine
kind: Type
declaration: "type TBlendLine = procedure(Src, Dst: PColor32; Count: Integer);"
summary: "Procedural delegate for blending a source pixel scanline buffer onto a destination scanline buffer."
parameters:
  - name: Src
    type: PColor32
    description: "Pointer to the first source foreground pixel in memory."
  - name: Dst
    type: PColor32
    description: "Pointer to the first destination background pixel in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous pixels in the scanline buffers to blend."
seealso:
  - "[[BlendLine]]"
  - "[[MergeLine]]"
---

## Description

`TBlendLine` defines the signature for array/scanline buffer blending routines. It iterates through `Count` pairs of source (`Src`) and destination (`Dst`) pixels, blending each source pixel onto its corresponding destination pixel.

---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendLineEx
kind: Type
declaration: "type TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: Cardinal);"
summary: "Procedural delegate for blending scanline buffers with a constant master alpha weight or mask."
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
  - name: M
    type: Cardinal
    description: "Master alpha weight or mask value (0..255)."
seealso:
  - "[[BlendLineEx]]"
  - "[[MergeLineEx]]"
---

## Description

`TBlendLineEx` defines the signature for scanline buffer blending routines modulated by an additional master alpha factor or mask `M` ($0 \dots 255$).

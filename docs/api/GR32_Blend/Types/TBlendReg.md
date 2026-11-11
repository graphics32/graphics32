---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendReg
kind: Type
declaration: "type TBlendReg = function(F, B: TColor32): TColor32;"
summary: "Procedural delegate for blending two TColor32 pixel values in registers."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
returns:
  - type: TColor32
    description: "The blended 32-bit ARGB result color."
seealso:
  - "[[BlendReg]]"
  - "[[MergeReg]]"
---

## Description

`TBlendReg` defines the signature for single-pixel register blending functions. It takes a foreground color `F` and background color `B`, computes the blended color result according to alpha rules or blend mode formulas, and returns the result `TColor32`.

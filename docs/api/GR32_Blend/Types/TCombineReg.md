---
layout: doc
docType: api
unit: GR32_Blend
entity: TCombineReg
kind: Type
declaration: "type TCombineReg = function(X, Y: TColor32; W: Cardinal): TColor32;"
summary: "Procedural delegate for linear interpolation (Lerp) between two colors in registers."
parameters:
  - name: X
    type: TColor32
    description: "First color value (weight W)."
  - name: Y
    type: TColor32
    description: "Second color value (weight 255 - W)."
  - name: W
    type: Cardinal
    description: "Interpolation weight (0..255)."
returns:
  - type: TColor32
    description: "The combined 32-bit ARGB result color."
seealso:
  - "[[CombineReg]]"
  - "[[CombineMem]]"
---

## Description

`TCombineReg` defines the signature for register linear combination (Lerp) functions:
$$Z = W \cdot X + (1 - W) \cdot Y$$
All four color channels (A, R, G, B) are interpolated linearly.

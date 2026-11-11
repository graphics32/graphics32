---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendRegEx
kind: Type
declaration: "type TBlendRegEx = function(F, B: TColor32; M: Cardinal): TColor32;"
summary: "Procedural delegate for register blending with an additional master alpha/mask weight."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
  - name: M
    type: Cardinal
    description: "Master alpha weight or mask value (0..255)."
returns:
  - type: TColor32
    description: "The blended 32-bit ARGB result color."
seealso:
  - "[[BlendRegEx]]"
  - "[[MergeRegEx]]"
---

## Description

`TBlendRegEx` defines the signature for register blending routines that modulate foreground alpha with an additional master alpha / mask factor `M` ($0 \dots 255$).

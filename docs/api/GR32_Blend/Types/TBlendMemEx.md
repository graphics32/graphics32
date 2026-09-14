---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendMemEx
kind: Type
declaration: "type TBlendMemEx = procedure(F: TColor32; var B: TColor32; M: Cardinal);"
summary: "Procedural delegate for memory blending with an additional master alpha/mask weight."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Reference to the background pixel variable to be updated in-place."
  - name: M
    type: Cardinal
    description: "Master alpha weight or mask value (0..255)."
seealso:
  - "[[BlendMemEx]]"
  - "[[MergeMemEx]]"
---

## Description

`TBlendMemEx` defines the signature for memory-in-place pixel blending routines using an additional master alpha weight or mask `M` ($0 \dots 255$).

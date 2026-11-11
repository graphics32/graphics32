---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendMem
kind: Type
declaration: "type TBlendMem = procedure(F: TColor32; var B: TColor32);"
summary: "Procedural delegate for blending a foreground color into a single in-memory background pixel."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Reference to the background pixel variable to be updated in-place."
seealso:
  - "[[BlendMem]]"
  - "[[MergeMem]]"
---

## Description

`TBlendMem` defines the signature for memory-in-place pixel blending functions. It blends the foreground color `F` into the background pixel referenced by variable `B` and overwrites `B` in-place with the resulting `TColor32`.

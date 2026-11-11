---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendMems
kind: Type
declaration: "type TBlendMems = procedure(F: TColor32; B: PColor32; Count: Integer);"
summary: "Procedural delegate for blending a single foreground color into a contiguous array of background pixels."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: PColor32
    description: "Pointer to the first destination background pixel in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous background pixels to blend."
seealso:
  - "[[BlendMems]]"
  - "[[MergeMems]]"
---

## Description

`TBlendMems` defines the signature for vector/array memory-in-place blending operations where a uniform foreground color `F` is blended across `Count` contiguous background pixels starting at `B`.

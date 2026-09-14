---
layout: doc
docType: api
unit: GR32_Blend
entity: TPremultiplyMem
kind: Type
declaration: "type TPremultiplyMem = procedure(Pixels: PColor32; Count: Integer);"
summary: "Procedural delegate for batch premultiplying or unpremultiplying alpha on a buffer of pixels."
parameters:
  - name: Pixels
    type: PColor32
    description: "Pointer to the first 32-bit ARGB pixel in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous pixels in the buffer to process."
seealso:
  - "[[PremultiplyMem]]"
  - "[[UnpremultiplyMem]]"
---

## Description

`TPremultiplyMem` defines the signature for batch alpha format conversion procedures that iterate over `Count` contiguous pixels starting at `Pixels` to apply alpha premultiplication or unpremultiplication.

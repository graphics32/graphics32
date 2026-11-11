---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendMemRGB
kind: Type
declaration: "type TBlendMemRGB = procedure(F: TColor32; var B: TColor32; W: Cardinal);"
summary: "Procedural delegate for in-memory blending with an explicit constant blending weight."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Reference to the background pixel variable to be updated in-place."
  - name: W
    type: Cardinal
    description: "Explicit blending weight (0..255)."
seealso:
  - "[[BlendMemRGB]]"
  - "[[BlendRegRGB]]"
---

## Description

`TBlendMemRGB` defines the signature for in-memory pixel blending routines where blending weight `W` ($0 \dots 255$) is passed explicitly to blend `F` into background pixel `B`.

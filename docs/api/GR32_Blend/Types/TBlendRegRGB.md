---
layout: doc
docType: api
unit: GR32_Blend
entity: TBlendRegRGB
kind: Type
declaration: "type TBlendRegRGB = function(F, B: TColor32; W: Cardinal): TColor32;"
summary: "Procedural delegate for blending foreground and background colors with explicit constant RGB weight."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
  - name: W
    type: Cardinal
    description: "Explicit blending weight (0..255)."
returns:
  - type: TColor32
    description: "The blended 32-bit ARGB result color."
seealso:
  - "[[BlendRegRGB]]"
  - "[[BlendMemRGB]]"
---

## Description

`TBlendRegRGB` defines the signature for register blending routines where blending weight `W` ($0 \dots 255$) is provided explicitly rather than extracted from `F`'s alpha channel.

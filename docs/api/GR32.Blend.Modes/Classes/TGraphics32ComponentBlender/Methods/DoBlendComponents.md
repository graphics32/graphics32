---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TGraphics32ComponentBlender
entity: TGraphics32ComponentBlender.DoBlendComponents
kind: Method
declaration: "class function DoBlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal; Blended: TColor32): TColor32; static;"
summary: "Applies Adobe Photoshop alpha compositing across all RGB color channels."
parameters:
  - name: fColor
    type: TColor32
    description: "Foreground RGB color (alpha ignored)."
  - name: fAlpha
    type: Cardinal
    description: "Foreground alpha (0..255)."
  - name: bColor
    type: TColor32
    description: "Background RGB color (alpha ignored)."
  - name: bAlpha
    type: Cardinal
    description: "Background alpha (0..255)."
  - name: Blended
    type: TColor32
    description: "The blended RGB color resulting from channel blending."
returns:
  type: TColor32
  description: "The final composite 32-bit ARGB color."
---

## Description

`DoBlendComponents` calculates the overall result alpha ($rAlpha = fAlpha + bAlpha \cdot (1 - fAlpha)$) and passes each RGB channel (red, green, blue) through `DoBlendComponent` to compute the composite output pixel color.

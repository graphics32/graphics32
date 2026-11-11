---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TCustomGraphics32ComponentBlender
entity: TCustomGraphics32ComponentBlender.BlendComponents
kind: Method
declaration: "function BlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; virtual; abstract;"
summary: "Blends foreground and background color components given separated RGB and alpha values."
parameters:
  - name: fColor
    type: TColor32
    description: "Foreground RGB color (alpha ignored)."
  - name: fAlpha
    type: Cardinal
    description: "Foreground alpha value (0..255)."
  - name: bColor
    type: TColor32
    description: "Background RGB color (alpha ignored)."
  - name: bAlpha
    type: Cardinal
    description: "Background alpha value (0..255)."
returns:
  type: TColor32
  description: "The resulting blended 32-bit ARGB color."
---

## Description

`BlendComponents` is the abstract worker method overridden by derived component blenders to combine foreground RGB `fColor` and alpha `fAlpha` with background RGB `bColor` and alpha `bAlpha`.

---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TGraphics32SeparableBlender
entity: TGraphics32SeparableBlender.BlendComponents
kind: Method
declaration: "function BlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;"
summary: "Executes BlendComponent on each RGB channel and applies Adobe alpha compositing."
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
returns:
  type: TColor32
  description: "The composite 32-bit ARGB color."
---

## Description

`BlendComponents` overrides `TCustomGraphics32ComponentBlender.BlendComponents` to evaluate [[BlendComponent]] for red, green, and blue components individually and composite the results using `DoBlendComponent`.

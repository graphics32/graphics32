---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TGraphics32SeparableBlender
entity: TGraphics32SeparableBlender.BlendComponent
kind: Method
declaration: "class function BlendComponent(F, B: Cardinal): Cardinal; virtual; abstract;"
summary: "Blends a single foreground channel F and background channel B."
parameters:
  - name: F
    type: Cardinal
    description: "Foreground single channel value (0..255)."
  - name: B
    type: Cardinal
    description: "Background single channel value (0..255)."
returns:
  type: Cardinal
  description: "The blended channel result (0..255)."
---

## Description

`BlendComponent` is the abstract single-channel blend function implemented by subclasses to define specific separable blend mode math (such as Multiply, Screen, Overlay, Dodge, Burn, etc.).

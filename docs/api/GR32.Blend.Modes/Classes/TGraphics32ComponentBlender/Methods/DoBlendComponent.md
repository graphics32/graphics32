---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TGraphics32ComponentBlender
entity: TGraphics32ComponentBlender.DoBlendComponent
kind: Method
declaration: "class function DoBlendComponent(fColor, fAlpha, bColor, bAlpha, rAlpha, rfAlpha, Blended: Cardinal): Cardinal; static; inline;"
summary: "Calculates the resulting single color channel value using Adobe Photoshop alpha compositing."
parameters:
  - name: fColor
    type: Cardinal
    description: "Foreground single channel value (0..255)."
  - name: fAlpha
    type: Cardinal
    description: "Foreground alpha (0..255)."
  - name: bColor
    type: Cardinal
    description: "Background single channel value (0..255)."
  - name: bAlpha
    type: Cardinal
    description: "Background alpha (0..255)."
  - name: rAlpha
    type: Cardinal
    description: "Precomputed resulting total alpha."
  - name: rfAlpha
    type: Cardinal
    description: "Precomputed relative foreground alpha ratio (255 * fAlpha / rAlpha)."
  - name: Blended
    type: Cardinal
    description: "The blended channel value computed by the mode's specific blend function."
returns:
  type: Cardinal
  description: "The final composite single channel value (0..255)."
---

## Description

`DoBlendComponent` implements the channel-level Adobe compositing math:
$$rColor = (1 - fAlpha / rAlpha) \cdot bColor + (fAlpha / rAlpha) \cdot ((1 - bAlpha) \cdot fColor + bAlpha \cdot Blended)$$
It combines foreground, background, and mode-blended channel values according to their respective alpha transparencies.

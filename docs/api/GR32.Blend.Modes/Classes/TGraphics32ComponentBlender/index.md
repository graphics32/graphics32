---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: TGraphics32ComponentBlender
kind: Class
abstract: true
declaration: |
  TGraphics32ComponentBlender = class abstract(TCustomGraphics32ComponentBlender)
inheritance:
  - TObject
  - TCustomGraphics32Blender
  - TCustomGraphics32ComponentBlender
  - TGraphics32ComponentBlender
summary: "Abstract base class implementing the Adobe Photoshop alpha compositing formula."
---

## Description

`TGraphics32ComponentBlender` implements the standard Adobe / Photoshop compositing formula for multi-channel separable color blending. It provides helper methods `DoBlendComponent` and `DoBlendComponents` to calculate result alpha and channel blending.

[members]

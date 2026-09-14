---
layout: doc
docType: api
unit: GR32_Blend
entity: ColorAlgebra
aliases: [ColorAdd, ColorSub, ColorDiv, ColorModulate, ColorMax, ColorMin, ColorDifference, ColorAverage, ColorExclusion, ColorScale, ColorScreen, ColorDodge, ColorBurn, BlendColorAdd, BlendColorModulate]
kind: Function
declaration: |
  function ColorAdd(F, B: TColor32): TColor32;
  function ColorSub(F, B: TColor32): TColor32;
  function ColorDiv(F, B: TColor32): TColor32;
  function ColorModulate(F, B: TColor32): TColor32;
  function ColorMax(F, B: TColor32): TColor32;
  function ColorMin(F, B: TColor32): TColor32;
  function ColorDifference(F, B: TColor32): TColor32;
  function ColorAverage(F, B: TColor32): TColor32;
  function ColorExclusion(F, B: TColor32): TColor32;
  function ColorScale(F, B: TColor32): TColor32;
  function ColorScreen(F, B: TColor32): TColor32;
  function ColorDodge(F, B: TColor32): TColor32;
  function ColorBurn(F, B: TColor32): TColor32;
  function BlendColorAdd(F, B: TColor32): TColor32;
  function BlendColorModulate(F, B: TColor32): TColor32;
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
returns:
  - type: TColor32
    description: "The blended 32-bit ARGB result color."
summary: "Color channel arithmetic and Photoshop-style compositing blend mode delegates."
seealso:
  - "[[BlendReg]]"
  - "[[MergeReg]]"
  - "[[CombineReg]]"
---

## Description

This group of functions implements color channel arithmetic and Photoshop-style compositing blend modes between foreground color $F$ and background color $B$.

### Available Blend Modes

| Name | Formula / Operation | Description |
| --- | --- | --- |
| `ColorAdd` | $\min(255, F_c + B_c)$ | Additive blending with channel saturation. |
| `ColorSub` | $\max(0, B_c - F_c)$ | Subtractive blending with zero clamping. |
| `ColorDiv` | $\min(255, (B_c \cdot 255) / F_c)$ | Division of background by foreground. |
| `ColorModulate` | $(F_c \cdot B_c) / 255$ | Multiplicative modulation (darkening). |
| `ColorMax` | $\max(F_c, B_c)$ | Component-wise maximum color selection. |
| `ColorMin` | $\min(F_c, B_c)$ | Component-wise minimum color selection. |
| `ColorDifference` | $\|F_c - B_c\|$ | Absolute channel difference. |
| `ColorAverage` | $(F_c + B_c) / 2$ | Component-wise average. |
| `ColorExclusion` | $F_c + B_c - \frac{2 \cdot F_c \cdot B_c}{255}$ | Exclusion blend mode (soft contrast inversion). |
| `ColorScale` | $(F_c \cdot B_c) / 255$ | Scaled multiplication. |
| `ColorScreen` | $255 - \frac{(255 - F_c)(255 - B_c)}{255}$ | Screen blend mode (lightening blend). |
| `ColorDodge` | $\min(255, \frac{B_c \cdot 255}{255 - F_c})$ | Color dodge blend mode (brightening). |
| `ColorBurn` | $255 - \min(255, \frac{(255 - B_c) \cdot 255}{F_c})$ | Color burn blend mode (darkening). |
| `BlendColorAdd` | `ColorAdd` modulated by $F_a$ | Additive blending modulated by foreground alpha. |
| `BlendColorModulate` | `ColorModulate` modulated by $F_a$ | Multiplicative modulation modulated by foreground alpha. |

::: info
All these functions are actually delegates; At startup, Graphics32 [binds them to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
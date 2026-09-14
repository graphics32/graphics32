---
layout: doc
docType: api
unit: GR32
entity: ScaleAlpha
kind: Procedure
declaration: "procedure ScaleAlpha(var Color32: TColor32; Scale: Single);"
summary: "Scales the alpha transparency component of a TColor32 variable in-place by a floating-point factor."
parameters:
  - name: Color32
    type: TColor32
    description: "Reference to the 32-bit ARGB color variable to be updated."
  - name: Scale
    type: Single
    description: "Floating-point scale factor (e.g. 0.5 for 50% opacity)."
seealso:
  - "[[SetAlpha]]"
  - "[[ModifyAlpha]]"
  - "[[TColor32]]"
---

## Description

`ScaleAlpha` multiplies the alpha channel of `Color32` in-place by floating-point factor `Scale` and rounds the resulting value to the nearest integer:
$$A_{\text{new}} = \text{Round}(\text{Scale} \cdot A_{\text{old}})$$
The RGB color channels remain unchanged.

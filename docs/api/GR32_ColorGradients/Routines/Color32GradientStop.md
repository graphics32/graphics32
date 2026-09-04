---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: Color32GradientStop
kind: Function
declaration: "function Color32GradientStop(Offset: TFloat; Color: TColor32): TColor32GradientStop; overload;"
summary: "Constructs and initializes a TColor32GradientStop record from an offset position and color."
parameters:
  - name: Offset
    type: TFloat
    description: "Normalized gradient offset position in the range [0.0, 1.0]."
  - name: Color
    type: TColor32
    description: "32-bit ARGB color value."
returns:
  - type: TColor32GradientStop
    description: "A [[TColor32GradientStop]] record initialized with the specified offset and color."
---

## Description

`Color32GradientStop` is a convenience constructor routine for creating [[TColor32GradientStop]] records inline.

## See Also
- [[TColor32GradientStop]]
- [[TColor32Gradient]]

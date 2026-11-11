---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.AddColorStop
kind: Method
summary: "Adds a color stop entry to the gradient and re-sorts stops by offset position."
overloads:
  - signature: "procedure AddColorStop(Offset: TFloat; Color: TColor32); overload; virtual;"
    summary: "Adds a color stop specified by scalar offset position and color."
    parameters:
      - name: Offset
        type: TFloat
        description: "Normalized offset position along gradient domain in the range [0.0, 1.0]."
      - name: Color
        type: TColor32
        description: "32-bit ARGB color value."
  - signature: "procedure AddColorStop(ColorStop: TColor32GradientStop); overload; virtual;"
    summary: "Adds a color stop record."
    parameters:
      - name: ColorStop
        type: TColor32GradientStop
        description: "Color stop record containing Offset and Color32."
---

## Description

Inserts a new color stop into the gradient list. The method maintains color stops sorted in ascending order of `Offset`. If an existing stop has the exact same offset, its color is updated. Triggers `OnGradientColorsChanged`.

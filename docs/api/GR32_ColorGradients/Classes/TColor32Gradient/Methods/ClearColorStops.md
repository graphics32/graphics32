---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.ClearColorStops
kind: Method
summary: "Clears all color stops or resets gradient to a single solid color."
overloads:
  - signature: "procedure ClearColorStops; overload;"
    summary: "Removes all color stops from the gradient."
  - signature: "procedure ClearColorStops(Color: TColor32); overload;"
    summary: "Removes all color stops and adds two stops (offsets 0.0 and 1.0) with the specified solid Color."
    parameters:
      - name: Color
        type: TColor32
        description: "Solid 32-bit ARGB color value."
---

## Description

Resets or clears the internal color stop list and notifies attached listeners via `OnGradientColorsChanged`.

---
layout: doc
docType: api
unit: GR32
entity: HSVtoRGB
kind: Routine
summary: "Converts Hue, Saturation, and Value components into a 32-bit ARGB TColor32 color."
declaration: "function HSVtoRGB(H, S, V: Single; A: Integer = 255): TColor32;"
parameters:
  - name: H
    type: Single
    description: "Hue component in range [0..1]."
  - name: S
    type: Single
    description: "Saturation component in range [0..1]."
  - name: V
    type: Single
    description: "Value / Brightness component in range [0..1]."
  - name: A
    type: Integer
    description: "Alpha opacity channel value (0..255, defaults to 255 for opaque)."
returns:
  - type: TColor32
    description: "The 32-bit ARGB `TColor32` color converted from HSV inputs."
---

## Description

`HSVtoRGB` converts normalized Hue, Saturation, and Value floating-point inputs ($[0..1]$) into a 32-bit ARGB [[TColor32]] color.

The optional `A` parameter specifies the output alpha channel value ($0..255$). If omitted, it defaults to `255` (fully opaque).

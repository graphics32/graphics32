---
layout: doc
docType: api
unit: GR32
entity: RGBtoHSV
kind: Routine
summary: "Converts a 32-bit ARGB TColor32 value into Hue, Saturation, and Value components."
declaration: "procedure RGBToHSV(Color: TColor32; out H, S, V: Single);"
parameters:
  - name: Color
    type: TColor32
    description: "Input ARGB color."
  - name: H
    type: Single
    description: "Output Hue component in range [0..1]."
  - name: S
    type: Single
    description: "Output Saturation component in range [0..1]."
  - name: V
    type: Single
    description: "Output Value / Brightness component in range [0..1]."
---

## Description

`RGBToHSV` converts a [[TColor32]]color to normalized Hue ($H \in [0..1]$), Saturation ($S \in [0..1]$), and Value ($V \in [0..1]$) components.

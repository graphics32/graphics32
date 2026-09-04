---
layout: doc
docType: api
unit: GR32
entity: RedComponent
kind: Function
declaration: "function RedComponent(Color32: TColor32): Integer;"
summary: "Extracts the 8-bit Red channel component (0..255) from a TColor32 value."
parameters:
  - name: Color32
    type: TColor32
    description: "Source 32-bit ARGB color."
returns:
  - type: Integer
    description: "The red channel component value in range [0..255]."
---

## Description

`RedComponent` extracts the red channel value from a [[TColor32]] integer, returning a value between `0` and `255`.

---
layout: doc
docType: api
unit: GR32
entity: AlphaComponent
kind: Function
declaration: "function AlphaComponent(Color32: TColor32): Integer;"
summary: "Extracts the 8-bit Alpha channel component (0..255) from a TColor32 value."
parameters:
  - name: Color32
    type: TColor32
    description: "Source 32-bit ARGB color."
returns:
  - type: Integer
    description: "The alpha channel component value in range [0..255]."
---

## Description

`AlphaComponent` extracts the alpha channel value (opacity) from a [[TColor32]] integer, returning a value in the range `0` (fully transparent) to `255` (fully opaque).

---
layout: doc
docType: api
unit: GR32
entity: SetAlpha
kind: Function
declaration: "function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;"
summary: "Replaces the alpha channel of a TColor32 value with a new alpha value (0..255)."
parameters:
  - name: Color32
    type: TColor32
    description: "Original 32-bit ARGB color."
  - name: NewAlpha
    type: Integer
    description: "New alpha channel value (0..255)."
---

## Description

`SetAlpha` replaces the alpha channel of `Color32` with `NewAlpha`, returning the modified [[TColor32]] color.

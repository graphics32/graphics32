---
layout: doc
docType: api
unit: GR32
entity: ModifyAlpha
kind: Procedure
declaration: "procedure ModifyAlpha(var Color32: TColor32; NewAlpha: Byte);"
summary: "Modifies the alpha transparency component of a TColor32 variable directly in-place."
parameters:
  - name: Color32
    type: TColor32
    description: "Reference to the 32-bit ARGB color variable to be updated."
  - name: NewAlpha
    type: Byte
    description: "New alpha channel value (0..255)."
seealso:
  - "[[SetAlpha]]"
  - "[[ScaleAlpha]]"
  - "[[TColor32]]"
---

## Description

`ModifyAlpha` updates the alpha channel of `Color32` in-place using a 8-bit `Byte` value (`NewAlpha`), preserving the existing RGB color channels.

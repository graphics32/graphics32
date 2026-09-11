---
layout: doc
docType: api
unit: GR32_Gamma
entity: GAMMA_ENCODING_TABLE
kind: Variable
declaration: "var GAMMA_ENCODING_TABLE: TGammaTable8Bit;"
summary: "Global 256-entry lookup table for gamma encoding (linear light to gamma-compressed space)."
seealso:
  - "[[GAMMA_DECODING_TABLE]]"
  - "[[ApplyGamma]]"
  - "[[SetGamma]]"
  - "[[Set_sRGB]]"
---

## Description

`GAMMA_ENCODING_TABLE` is a global [[TGammaTable8Bit]] array precalculated to map linear light intensity values ($0 \dots 255$) to gamma-compressed output values ($0 \dots 255$).

It is initialized by [[SetGamma]] or [[Set_sRGB]] and used by [[ApplyGamma]] routines to encode single [[TColor32]] colors, pixel arrays, or entire bitmaps.

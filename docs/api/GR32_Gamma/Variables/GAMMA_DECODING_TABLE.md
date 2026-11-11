---
layout: doc
docType: api
unit: GR32_Gamma
entity: GAMMA_DECODING_TABLE
kind: Variable
declaration: "var GAMMA_DECODING_TABLE: TGammaTable8Bit;"
summary: "Global 256-entry lookup table for gamma decoding (gamma-compressed space to linear light)."
seealso:
  - "[[GAMMA_ENCODING_TABLE]]"
  - "[[ApplyInvGamma]]"
  - "[[SetGamma]]"
  - "[[Set_sRGB]]"
---

## Description

`GAMMA_DECODING_TABLE` is a global [[TGammaTable8Bit]] array precalculated to map gamma-compressed input values ($0 \dots 255$) to linear light intensity values ($0 \dots 255$).

It is initialized by [[SetGamma]] or [[Set_sRGB]] and used by [[ApplyInvGamma]] routines to decode single [[TColor32]] colors, pixel arrays, or entire bitmaps into linear light space.

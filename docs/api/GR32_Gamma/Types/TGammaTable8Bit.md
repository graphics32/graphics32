---
layout: doc
docType: api
unit: GR32_Gamma
entity: TGammaTable8Bit
kind: Type
declaration: "type TGammaTable8Bit = array [Byte] of Byte;"
summary: "256-byte lookup table array mapping 8-bit color channel values under gamma transformations."
seealso:
  - "[[GAMMA_ENCODING_TABLE]]"
  - "[[GAMMA_DECODING_TABLE]]"
  - "[[SetGamma]]"
  - "[[Set_sRGB]]"
---

## Description

`TGammaTable8Bit` represents a 256-entry lookup table mapping 8-bit input channel values ($0 \dots 255$) to 8-bit transformed channel output values ($0 \dots 255$).

It is used by [[GAMMA_ENCODING_TABLE]], [[GAMMA_DECODING_TABLE]], and routines such as [[SetGamma]], [[Set_sRGB]], and [[ApplyCustomGamma]] to perform fast $O(1)$ channel conversions without evaluating floating-point transcendental exponential functions per pixel.

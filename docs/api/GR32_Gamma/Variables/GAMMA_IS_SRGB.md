---
layout: doc
docType: api
unit: GR32_Gamma
entity: GAMMA_IS_SRGB
kind: Variable
declaration: "var GAMMA_IS_SRGB: Boolean;"
summary: "Indicates whether global gamma encoding and decoding tables currently use sRGB transfer functions."
seealso:
  - "[[Set_sRGB]]"
  - "[[SetGamma]]"
  - "[[GAMMA_VALUE]]"
---

## Description

`GAMMA_IS_SRGB` is a boolean flag indicating the current configuration state of global lookup tables [[GAMMA_ENCODING_TABLE]] and [[GAMMA_DECODING_TABLE]].

* When [[Set_sRGB]] is called, `GAMMA_IS_SRGB` is set to `True`, confirming that global tables contain sRGB IEC 61966-2-1 piecewise transfer function values.
* When [[SetGamma]] is called, `GAMMA_IS_SRGB` is set to `False`, indicating that global tables contain power-law gamma values based on [[GAMMA_VALUE]].

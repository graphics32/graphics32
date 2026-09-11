---
layout: doc
docType: api
unit: GR32_Gamma
entity: GAMMA_VALUE
kind: Variable
declaration: "var GAMMA_VALUE: Double;"
summary: "Contains the power-law gamma exponent value when sRGB mode is disabled."
seealso:
  - "[[GAMMA_IS_SRGB]]"
  - "[[SetGamma]]"
  - "[[DEFAULT_GAMMA]]"
---

## Description

`GAMMA_VALUE` holds the current floating-point power-law gamma exponent $\gamma$ when `GAMMA_IS_SRGB` is `False`.

Calling [[SetGamma]] updates `GAMMA_VALUE` with the provided exponent (or [[DEFAULT_GAMMA]]) and populates [[GAMMA_ENCODING_TABLE]] and [[GAMMA_DECODING_TABLE]].

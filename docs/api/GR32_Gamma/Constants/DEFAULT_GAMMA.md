---
layout: doc
docType: api
unit: GR32_Gamma
entity: DEFAULT_GAMMA
kind: Constant
declaration: "const DEFAULT_GAMMA: Double = 1.6;"
summary: "Default gamma exponent value (1.6) used for line and pixel antialiasing bias."
seealso:
  - "[[SetGamma]]"
  - "[[GAMMA_VALUE]]"
---

## Description

`DEFAULT_GAMMA` defines the default power-law gamma exponent value ($1.6$).

In Graphics32, calling [[SetGamma]] without arguments uses `DEFAULT_GAMMA` to populate the global encoding and decoding tables. This exponent value of $1.6$ is historically tuned in Graphics32 to provide an optimal antialiasing bias compensation for line drawing and polygon rasterization routines.

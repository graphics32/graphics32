---
layout: doc
docType: api
unit: GR32_Blend
entity: TLightenReg
kind: Type
declaration: "type TLightenReg = function(C: TColor32; Amount: Integer): TColor32;"
summary: "Procedural delegate for adjusting pixel brightness by a specified offset."
parameters:
  - name: C
    type: TColor32
    description: "Input pixel color."
  - name: Amount
    type: Integer
    description: "Brightness offset amount (-255..255)."
returns:
  - type: TColor32
    description: "The brightness-adjusted 32-bit ARGB result color."
seealso:
  - "[[LightenReg]]"
  - "[[ScaleMems]]"
---

## Description

`TLightenReg` defines the signature for register brightness adjustment routines. Positive `Amount` values brighten RGB components (clamping to 255), while negative values darken RGB components (clamping to 0).

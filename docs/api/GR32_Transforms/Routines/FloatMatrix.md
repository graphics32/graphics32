---
layout: doc
docType: api
unit: GR32_Transforms
entity: FloatMatrix
kind: Function
declaration: "function FloatMatrix(const FixedMatrix: TFixedMatrix): TFloatMatrix; overload;"
summary: "Converts a 3x3 16.16 fixed-point matrix to floating-point matrix format."
parameters:
  - name: FixedMatrix
    type: TFixedMatrix
    description: "Input 3x3 fixed-point matrix."
returns:
  - type: TFloatMatrix
    description: "The converted [[TFloatMatrix]] floating-point matrix."
---

## Description

`FloatMatrix` converts a 3x3 16.16 fixed-point matrix (`TFixedMatrix`) into a floating-point matrix (`TFloatMatrix`) by multiplying each element by `FixedToFloat` ($1 / 65536$).

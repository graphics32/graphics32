---
layout: doc
docType: api
unit: GR32_Transforms
entity: FixedMatrix
kind: Function
declaration: "function FixedMatrix(const FloatMatrix: TFloatMatrix): TFixedMatrix; overload;"
summary: "Converts a 3x3 floating-point matrix to 16.16 fixed-point matrix format."
parameters:
  - name: FloatMatrix
    type: TFloatMatrix
    description: "Input 3x3 floating-point matrix."
---

## Description

`FixedMatrix` converts a 3x3 floating-point matrix (`TFloatMatrix`) into a 16.16 fixed-point matrix (`TFixedMatrix`) by scaling each matrix element by $65536$ (`FixedOne`) and rounding to integer precision.

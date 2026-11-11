---
layout: doc
docType: api
unit: GR32_Transforms
entity: Scale
kind: Procedure
declaration: "procedure Scale(var M: TFloatMatrix; Factor: TFloat);"
summary: "Scales all elements of a 3x3 matrix by a scalar factor in place."
parameters:
  - name: M
    type: TFloatMatrix
    description: "Matrix to scale in place."
  - name: Factor
    type: TFloat
    description: "Scalar multiplication factor."
---

## Description

`Scale` multiplies every entry $M_{i,j}$ in matrix `M` by `Factor` in place.

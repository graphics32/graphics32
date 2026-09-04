---
layout: doc
docType: api
unit: GR32_Transforms
entity: Determinant
kind: Function
declaration: "function Determinant(const M: TFloatMatrix): TFloat;"
summary: "Computes the determinant of a 3x3 floating-point matrix."
parameters:
  - name: M
    type: TFloatMatrix
    description: "Input 3x3 matrix."
returns:
  - type: TFloat
    description: |
      The scalar determinant $\det(M)$ of matrix `M`.
---

## Description

`Determinant` calculates the scalar determinant $\det(M)$ of a $3 \times 3$ floating-point matrix `M`.

---
layout: doc
docType: api
unit: GR32_Transforms
entity: Invert
kind: Procedure
declaration: "procedure Invert(var M: TFloatMatrix);"
summary: "Inverts a 3x3 floating-point matrix in place."
parameters:
  - name: M
    type: TFloatMatrix
    description: "Matrix to invert in place."
---

## Description

`Invert` calculates the inverse matrix $M^{-1}$ in place using the classical adjoint method divided by the matrix determinant ($\text{adj}(M) / \det(M)$). If $|\det(M)| < 10^{-5}$ (near-singular matrix), `M` is reset to `IdentityMatrix`.

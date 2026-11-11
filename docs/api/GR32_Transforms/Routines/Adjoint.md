---
layout: doc
docType: api
unit: GR32_Transforms
entity: Adjoint
kind: Procedure
declaration: "procedure Adjoint(var M: TFloatMatrix);"
summary: "Computes the classical adjoint (adjugate) matrix of a 3x3 floating-point matrix in place."
parameters:
  - name: M
    type: TFloatMatrix
    description: "Matrix to be replaced with its adjugate matrix."
---

## Description

`Adjoint` calculates the adjugate (transpose of the cofactor matrix) of a $3 \times 3$ floating-point matrix `M` in place.

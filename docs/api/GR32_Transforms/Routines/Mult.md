---
layout: doc
docType: api
unit: GR32_Transforms
entity: Mult
kind: Function
declaration: "function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;"
summary: "Multiplies two 3x3 floating-point matrices."
parameters:
  - name: M1
    type: TFloatMatrix
    description: "Left factor matrix."
  - name: M2
    type: TFloatMatrix
    description: "Right factor matrix."
---

## Description

`Mult` returns the matrix product $M_1 \times M_2$ of two $3 \times 3$ floating-point matrices.

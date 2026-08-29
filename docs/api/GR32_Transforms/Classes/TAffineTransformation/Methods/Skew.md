---
layout: doc
docType: api
unit: GR32_Transforms
parent: TAffineTransformation
entity: TAffineTransformation.Skew
kind: Method
declaration: "procedure Skew(Fx, Fy: TFloat);"
summary: "Applies shear (skew) factors to the transformation matrix."
parameters:
  - name: Fx, Fy
    type: TFloat
    description: "Horizontal and vertical shear factors."
---

## Description

`Skew` applies horizontal (`Fx`) and vertical (`Fy`) shear factors to the transformation matrix.

$$M = \begin{bmatrix} 1 & Fx & 0 \\ Fy & 1 & 0 \\ 0 & 0 & 1 \end{bmatrix} M;$$
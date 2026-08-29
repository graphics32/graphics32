---
layout: doc
docType: api
unit: GR32_Transforms
parent: TAffineTransformation
entity: TAffineTransformation.Translate
kind: Method
declaration: "procedure Translate(Dx, Dy: TFloat);"
summary: "Applies coordinate translation offsets along X and Y axes."
parameters:
  - name: Dx, Dy
    type: TFloat
    description: "Translation offsets along X and Y axes."
---

## Description

`Translate` multiplies the current matrix by a translation matrix offset $[Dx, Dy]$.

$$M = \begin{bmatrix} 1 & 0 & D_x \\ 0 & 1 & D_y \\ 0 & 0 & 1 \end{bmatrix} M;$$
---
layout: doc
docType: api
unit: GR32_Transforms
parent: TAffineTransformation
entity: TAffineTransformation.Scale
kind: Method
summary: "Applies 2D scaling factors to the transformation matrix."
overloads:
  - signature: "procedure Scale(Sx, Sy: TFloat); overload;"
    summary: "Scales coordinates independently along X and Y axes by Sx and Sy."
    parameters:
      - name: Sx, Sy
        type: TFloat
        description: "Scaling factors for X and Y axes."

  - signature: "procedure Scale(Value: TFloat); overload;"
    summary: "Applies uniform scaling along both X and Y axes."
    parameters:
      - name: Value
        type: TFloat
        description: "Uniform scaling factor."
---

## Description

`Scale` multiplies the current matrix by a scaling factor.

$$M = \begin{bmatrix} s_x & 0 & 0 \\ 0 & s_y & 0 \\ 0 & 0 & 1 \end{bmatrix} M;$$
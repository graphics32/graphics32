---
layout: doc
docType: api
unit: GR32_Transforms
parent: TAffineTransformation
entity: TAffineTransformation.Rotate
kind: Method
summary: "Applies a 2D rotation in degrees to the current transformation matrix."
overloads:
  - signature: "procedure Rotate(Alpha: TFloat); overload;"
    summary: "Rotates coordinates around the origin (0, 0) by Alpha degrees."
    parameters:
      - name: Alpha
        type: TFloat
        description: "Rotation angle in degrees."

  - signature: "procedure Rotate(Cx, Cy, Alpha: TFloat); overload;"
    summary: "Rotates coordinates around pivot point (Cx, Cy) by Alpha degrees."
    parameters:
      - name: Cx, Cy
        type: TFloat
        description: "Pivot point coordinates."
      - name: Alpha
        type: TFloat
        description: "Rotation angle in degrees."
---

## Description

`Rotate` multiplies the transformation matrix by a 2D rotation matrix for the specified angle in degrees.

At first, the origin is *translated* to (Cx, Cy), then an Alpha degree rotation is performed around the origin, and finally the origin is translated back.

$$M = \begin{bmatrix} \cos\alpha & \sin\alpha & 0 \\ -\sin\alpha & \cos\alpha & 0 \\ 0 & 0 & 1 \end{bmatrix} M;$$

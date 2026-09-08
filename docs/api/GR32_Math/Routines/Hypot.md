---
layout: doc
docType: api
unit: GR32_Math
entity: Hypot
kind: Function
summary: "Calculates the hypotenuse length sqrt(X^2 + Y^2)."
overloads:
  - signature: "function Hypot(const X, Y: TFloat): TFloat; overload;"
    summary: "Computes the floating-point hypotenuse for coordinates X and Y."
    parameters:
      - name: X
        type: TFloat
        description: "X coordinate."
      - name: Y
        type: TFloat
        description: "Y coordinate."
    returns:
      - type: TFloat
        description: "The floating-point hypotenuse distance."

  - signature: "function Hypot(const X, Y: Integer): Integer; overload;"
    summary: "Computes the integer hypotenuse rounded to the nearest integer."
    parameters:
      - name: X
        type: Integer
        description: "X coordinate."
      - name: Y
        type: Integer
        description: "Y coordinate."
    returns:
      - type: Integer
        description: "The rounded integer hypotenuse distance."
seealso:
  - "[[SinCos]]"
  - "[[FastSqrt]]"
---

## Description

`Hypot` calculates the length of the hypotenuse of a right-angled triangle with sides of length `X` and `Y`, corresponding to Euclidean distance $\sqrt{X^2 + Y^2}$.

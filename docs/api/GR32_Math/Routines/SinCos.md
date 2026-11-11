---
layout: doc
docType: api
unit: GR32_Math
entity: SinCos
kind: Procedure
summary: "Simultaneously calculates sine and cosine for an angle."
overloads:
  - signature: "procedure SinCos(const Theta: TFloat; out Sin, Cos: TFloat); overload;"
    summary: "Calculates the sine and cosine of angle Theta."
    parameters:
      - name: Theta
        type: TFloat
        description: "Angle in radians."
      - name: Sin
        type: TFloat
        description: "Output sine value."
      - name: Cos
        type: TFloat
        description: "Output cosine value."

  - signature: "procedure SinCos(const Theta, Radius: Single; out Sin, Cos: Single); overload;"
    summary: "Calculates sine and cosine scaled by Radius."
    parameters:
      - name: Theta
        type: Single
        description: "Angle in radians."
      - name: Radius
        type: Single
        description: "Scaling radius factor."
      - name: Sin
        type: Single
        description: "Output scaled sine value (Sin(Theta) * Radius)."
      - name: Cos
        type: Single
        description: "Output scaled cosine value (Cos(Theta) * Radius)."

  - signature: "procedure SinCos(const Theta, ScaleX, ScaleY: TFloat; out Sin, Cos: Single); overload;"
    summary: "Calculates sine scaled by ScaleX and cosine scaled by ScaleY."
    parameters:
      - name: Theta
        type: TFloat
        description: "Angle in radians."
      - name: ScaleX
        type: TFloat
        description: "Scale factor for sine."
      - name: ScaleY
        type: TFloat
        description: "Scale factor for cosine."
      - name: Sin
        type: Single
        description: "Output scaled sine value (Sin(Theta) * ScaleX)."
      - name: Cos
        type: Single
        description: "Output scaled cosine value (Cos(Theta) * ScaleY)."
seealso:
  - "[[Hypot]]"
---

## Description

`SinCos` computes both sine and cosine of angle `Theta` (in radians) in a single call, which is faster than computing each separately on CPU/FPU architectures.

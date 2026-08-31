---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomSparsePointGradientSampler
entity: TCustomSparsePointGradientSampler.SetColorPoints
kind: Method
summary: "Sets scatter vertices and colors."
overloads:
  - signature: "procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); overload; virtual; abstract;"
    summary: "Sets vertices from an array of TColor32FloatPoint records."
    parameters:
      - name: ColorPoints
        type: TArrayOfColor32FloatPoint
        description: "Array of position/color records."
  - signature: "procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); overload; virtual; abstract;"
    summary: "Sets vertices from separate position and color arrays."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Array of positions."
      - name: Colors
        type: TArrayOfColor32
        description: "Array of colors."
---

## Description

Configures scatter vertex positions and colors for sparse point gradient interpolators.

---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TColor32FloatPoint
kind: Type
aliases: [TArrayOfColor32FloatPoint]
declaration: |
  TColor32FloatPoint = record
    Point: TFloatPoint;
    Color32: TColor32;
  end;
  TArrayOfColor32FloatPoint = array of TColor32FloatPoint;
summary: "Associates a 2D floating-point coordinate point with a 32-bit ARGB color value for sparse point gradients."
---

## Description

`TColor32FloatPoint` couples a 2D geometric position (`Point`) with a color value (`Color32`). It serves as the fundamental vertex input for sparse-point color gradient interpolators (such as barycentric, bilinear, inverted distance weighting, Voronoi, and Delaunay triangulation samplers and fillers).

## Record Fields

| Field | Type | Description |
| --- | --- | --- |
| `Point` | [[TFloatPoint]] | 2D coordinate position $(X, Y)$ in floating-point precision. |
| `Color32` | [[TColor32]] | 32-bit ARGB color value anchored at this point coordinate. |

## See Also
- [[TCustomSparsePointGradientSampler]]
- [[Color32FloatPoint]]

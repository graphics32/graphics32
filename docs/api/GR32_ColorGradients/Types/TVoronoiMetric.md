---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TVoronoiMetric
kind: Type
aliases: [TVoronoiMetricFunc]
declaration: |
  TVoronoiMetric = (vmEuclidean, vmManhattan, vmCustom);
  TVoronoiMetricFunc = function (X, Y: TFloat; Point: TFloatPoint): TFloat;
summary: "Defines the distance calculation metric used by Voronoi cell gradient samplers."
---

## Description

`TVoronoiMetric` specifies the mathematical metric used by [[TVoronoiSampler]] to determine the distance between a sampling point $(X, Y)$ and feature site points in a Voronoi diagram.

## Enumeration Values

| Value | Description |
| --- | --- |
| `vmEuclidean` | Calculates standard straight-line distance $d = \sqrt{(X - P_x)^2 + (Y - P_y)^2}$. |
| `vmManhattan` | Calculates taxicab distance $d = \|X - P_x\| + \|Y - P_y\|$. |
| `vmCustom` | Uses a user-provided custom metric delegate function (`TVoronoiMetricFunc`). |

## Procedural Delegate

`TVoronoiMetricFunc` allows custom distance metric functions to be passed to [[TVoronoiSampler]].

## See Also
- [[TVoronoiSampler]]

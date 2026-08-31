---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TVoronoiSampler
entity: TVoronoiSampler.Metric
kind: Property
declaration: "property Metric: TVoronoiMetric read FMetric write SetMetric;"
summary: "Distance calculation metric (Euclidean, Manhattan, Custom)."
---

## Description

Controls cell boundary distance calculation:
- `vmEuclidean`: Standard straight line Euclidean distance.
- `vmManhattan`: Taxicab L1 distance resulting in diamond-shaped Voronoi cells.
- `vmCustom`: Custom delegate function provided via `MetricFunc`.

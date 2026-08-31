---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TVoronoiSampler
kind: Class
summary: "A sparse point Voronoi tesselation sampler."
declaration: "TVoronoiSampler = class(TCustomArbitrarySparsePointGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
  - TCustomArbitrarySparsePointGradientSampler
  - TVoronoiSampler
---

## Description

`TVoronoiSampler` samples discrete or cell-bounded color regions using Voronoi diagram metric distance partitioning.

Although the *Voronoi tesselation* can be used as a sparse point color gradient interpolator, it is actually rather a tesselation to discrete solid colors than a color gradient interpolation. However, it might still be relevant for several reasons; Compared to the [[TInvertedDistanceWeightingSampler]], the `TVoronoiSampler` will be the destination for high power values. At the same time it is related to Delaunay triangulation, which can be used in combination with barycentric interpolation for smooth color gradients.

As the Voronoi sampler typically does not contain any algorithm for antialiasing itself, use of a [[TSuperSampler]] might be required for smooth edges

| Direct | Supersampled |
| --- | --- |
| ![](/images/gradient-sampler-voronoi-direct.png) | ![](/images/gradient-sampler-voronoi-supersampled.png) |


## Mathematics & Algorithm

Given $N$ seed site points $P_i$ with colors $C_i$, `TVoronoiSampler` partitions space into Voronoi cells $V_i$:

$$V_i = \{ P \mid d(P, P_i) \le d(P, P_j) \;\forall j \ne i \}$$

The metric function $d(P, P_i)$ is selected via `Metric` ([[TVoronoiMetric]]):
- **Euclidean**: $d = \sqrt{(X - X_i)^2 + (Y - Y_i)^2}$
- **Manhattan**: $d = |X - X_i| + |Y - Y_i|$
- **Custom**: Evaluated via [[MetricFunc]].

Sampling point $P$ returns color $C_k$ of the nearest site $P_k$.

## References
- [Voronoi Diagram - Wikipedia](https://en.wikipedia.org/wiki/Voronoi_diagram)

[members]

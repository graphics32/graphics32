---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TGourandShadedDelaunayTrianglesSampler
kind: Class
summary: "Gourand shaded Delaunay triangles sampler."
declaration: "TGourandShadedDelaunayTrianglesSampler = class(TCustomArbitrarySparsePointGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
  - TCustomArbitrarySparsePointGradientSampler
  - TGourandShadedDelaunayTrianglesSampler
---

## Description

`TGourandShadedDelaunayTrianglesSampler` samples smooth color gradients across irregular 2D point clouds using Delaunay triangulation and Gouraud triangle shading.

<!-- TODO: more description -->

![](/images/gradient-sampler-gourand-ghaded-delaunay-triangles.png)

## Mathematics & Algorithm

`TGourandShadedDelaunayTrianglesSampler` constructs a Delaunay triangulation network over input point sets $P_i$.

For any sampling point $P$:
1. The enclosing triangle $(P_1, P_2, P_3)$ is located.
2. Gouraud shading is performed across the triangle using barycentric color interpolation:

$$C(P) = \lambda_1 C_1 + \lambda_2 C_2 + \lambda_3 C_3$$

## References
- [Delaunay Triangulation - Wikipedia](https://en.wikipedia.org/wiki/Delaunay_triangulation)
- [Gouraud Shading - Wikipedia](https://en.wikipedia.org/wiki/Gouraud_shading)

[members]

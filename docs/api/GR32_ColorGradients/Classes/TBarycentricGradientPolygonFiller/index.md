---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TBarycentricGradientPolygonFiller
kind: Class
summary: "Fills a polygon with 3-point barycentric linear color gradients."
declaration: "TBarycentricGradientPolygonFiller = class(TCustomSparsePointGradientPolygonFiller)"
inheritance:
  - TObject
  - TCustomPolygonFiller
  - TCustomSparsePointGradientPolygonFiller
  - TBarycentricGradientPolygonFiller
---

## Description

`TBarycentricGradientPolygonFiller` is polygon filler using a sparse point linear gradient interpolator using a barycentric coordinate system for interpolation.

Based on three (and only three) vertices, with each vertex specified with a certain color, a linear triangle of color is calculated. Typically the three vertices are mapped to the vertices of a given 3-point polygon that should be filled. However, this is not required as the colours extends outside this triangle.

![](/images/gradient-filler-barycentric.png)

## References
- [Barycentric Coordinate System - Wikipedia](https://en.wikipedia.org/wiki/Barycentric_coordinate_system)

## See also
- [[TBarycentricGradientSampler]]

[members]

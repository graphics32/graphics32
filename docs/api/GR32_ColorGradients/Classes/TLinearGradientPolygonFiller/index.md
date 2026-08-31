---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TLinearGradientPolygonFiller
kind: Class
summary: "High-speed linear gradient polygon scanline filler."
declaration: "TLinearGradientPolygonFiller = class(TCustomLinearGradientPolygonFiller)"
inheritance:
  - TObject
  - TCustomPolygonFiller
  - TCustomGradientPolygonFiller
  - TCustomGradientLookupTablePolygonFiller
  - TCustomLinearGradientPolygonFiller
  - TLinearGradientPolygonFiller
---

## Description

`TLinearGradientPolygonFiller` implements optimized scanline fillers for linear color gradients across polygons.

A linear color gradient is specified by a two or more points, and a color at each point.
The colors along the line through those points are calculated using linear interpolation, then extended *perpendicular* to that line.

![](/images/gradient-filler-linear.png)

[members]

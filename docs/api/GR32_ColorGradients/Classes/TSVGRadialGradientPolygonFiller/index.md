---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TSVGRadialGradientPolygonFiller
kind: Class
summary: "W3C SVG-compliant radial gradient filler."
declaration: "TSVGRadialGradientPolygonFiller = class(TCustomRadialGradientPolygonFiller)"
inheritance:
  - TObject
  - TCustomPolygonFiller
  - TCustomGradientPolygonFiller
  - TCustomGradientLookupTablePolygonFiller
  - TCustomRadialGradientPolygonFiller
  - TSVGRadialGradientPolygonFiller
---

## Description

`TSVGRadialGradientPolygonFiller` implements SVG-compliant radial gradient polygon rendering with focal point offset ([[FocalPoint]]).

The gradient is defined by specifying the [[EllipseBounds|ellipse bounds]] and the [[FocalPoint|focal point]]. Between this focal point and the ellipse, the colors are interpolated linearly according to the gradient color stops.

![](/images/gradient-filler-svg-radial.png)

<!-- TODO: How does  SVG radial gradients differ from ordinary radial gradients? -->

## References
- [SVG Radial Gradients](https://www.w3schools.com/graphics/svg_grad_radial.asp)

[members]

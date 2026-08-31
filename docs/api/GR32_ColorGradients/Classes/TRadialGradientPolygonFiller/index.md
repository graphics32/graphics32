---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TRadialGradientPolygonFiller
kind: Class
summary: "High-speed circular or elliptical radial gradient polygon filler."
declaration: "TRadialGradientPolygonFiller = class(TCustomRadialGradientPolygonFiller)"
inheritance:
  - TObject
  - TCustomPolygonFiller
  - TCustomGradientPolygonFiller
  - TCustomGradientLookupTablePolygonFiller
  - TCustomRadialGradientPolygonFiller
  - TRadialGradientPolygonFiller
---

## Description

`TRadialGradientPolygonFiller` fills polygons with radial gradients defined either as a **circle**, using [[Center]] and [[Radius]], or as an **ellipse** using [[ EllipseBounds]].

<!-- TODO: more description -->

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-filler-radial-clamp.png) | ![](/images/gradient-filler-radial-mirror.png) | ![](/images/gradient-filler-radial-repeat.png) |

Colors are mapped according to [[WrapMode]].

[members]

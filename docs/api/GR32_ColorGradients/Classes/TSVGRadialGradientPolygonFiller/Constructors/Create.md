---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TSVGRadialGradientPolygonFiller
entity: TSVGRadialGradientPolygonFiller.Create
kind: Constructor
summary: "Initializes a TSVGRadialGradientPolygonFiller instance."
overloads:
  - signature: "constructor Create(EllipseBounds: TFloatRect); overload;"
    summary: "Creates SVG radial filler with ellipse bounding rectangle."
    parameters:
      - name: EllipseBounds
        type: TFloatRect
        description: "Ellipse bounding rectangle."
  - signature: "constructor Create(EllipseBounds: TFloatRect; FocalPoint: TFloatPoint); overload;"
    summary: "Creates SVG radial filler with ellipse bounds and focal point offset."
    parameters:
      - name: EllipseBounds
        type: TFloatRect
        description: "Ellipse bounding rectangle."
      - name: FocalPoint
        type: TFloatPoint
        description: "Focal point coordinate."
---

## Description

Constructs an SVG-compliant radial polygon gradient filler instance.

---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TSVGRadialGradientPolygonFiller
entity: TSVGRadialGradientPolygonFiller.SetParameters
kind: Method
summary: "Configures SVG radial gradient bounds and focal point."
overloads:
  - signature: "procedure SetParameters(EllipseBounds: TFloatRect); overload;"
    summary: "Sets ellipse bounding rectangle."
    parameters:
      - name: EllipseBounds
        type: TFloatRect
        description: "Ellipse bounding rectangle."
  - signature: "procedure SetParameters(EllipseBounds: TFloatRect; FocalPoint: TFloatPoint); overload;"
    summary: "Sets ellipse bounds and focal point offset."
    parameters:
      - name: EllipseBounds
        type: TFloatRect
        description: "Ellipse bounding rectangle."
      - name: FocalPoint
        type: TFloatPoint
        description: "Focal point coordinate."
---

## Description

Updates SVG radial gradient parameters and recalculates focal transformation matrices.

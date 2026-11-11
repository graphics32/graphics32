---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TRadialGradientPolygonFiller
entity: TRadialGradientPolygonFiller.Create
kind: Constructor
summary: "Initializes a TRadialGradientPolygonFiller instance."
overloads:
  - signature: "constructor Create(Radius: TFloatPoint); overload;"
    summary: "Creates filler with radial bounds."
    parameters:
      - name: Radius
        type: TFloatPoint
        description: "Horizontal and vertical radii."
  - signature: "constructor Create(BoundingBox: TFloatRect); overload;"
    summary: "Creates filler with bounding box rectangle."
    parameters:
      - name: BoundingBox
        type: TFloatRect",
        description: "Bounding box rectangle."
  - signature: "constructor Create(Radius, Center: TFloatPoint); overload;"
    summary: "Creates filler with radius and center position."
    parameters:
      - name: Radius
        type: TFloatPoint
        description: "Horizontal and vertical radii."
      - name: Center
        type: TFloatPoint
        description: "Center position."
---

## Description

Constructs a radial polygon gradient filler instance.

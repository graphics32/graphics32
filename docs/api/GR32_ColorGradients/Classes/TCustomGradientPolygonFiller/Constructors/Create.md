---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomGradientPolygonFiller
entity: TCustomGradientPolygonFiller.Create
kind: Constructor
summary: "Initializes a TCustomGradientPolygonFiller instance."
overloads:
  - signature: "constructor Create; overload;"
    summary: "Creates filler with default parameters."
  - signature: "constructor Create(ColorGradient: TColor32Gradient); overload; virtual;"
    summary: "Creates filler with specified color gradient."
    parameters:
      - name: ColorGradient
        type: TColor32Gradient
        description: "Color gradient source."
---

## Description

Constructs a new polygon gradient filler instance.

---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TLinearGradientPolygonFiller
entity: TLinearGradientPolygonFiller.Create
kind: Constructor
summary: "Initializes a TLinearGradientPolygonFiller instance."
overloads:
  - signature: "constructor Create(ColorGradient: TColor32Gradient); overload; override;"
    summary: "Creates linear filler with specified color gradient."
    parameters:
      - name: ColorGradient
        type: TColor32Gradient
        description: "Color gradient."
  - signature: "constructor Create(ColorGradient: TColor32Gradient; UseLookupTable: Boolean); overload; virtual;"
    summary: "Creates linear filler specifying lookup table usage."
    parameters:
      - name: ColorGradient
        type: TColor32Gradient
        description: "Color gradient."
      - name: UseLookupTable
        type: Boolean
        description: "Lookup table toggle."
---

## Description

Constructs a linear polygon gradient filler instance.

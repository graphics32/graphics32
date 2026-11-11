---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomGradientSampler
entity: TCustomGradientSampler.Create
kind: Constructor
summary: "Initializes a TCustomGradientSampler instance with wrap mode or color gradient."
overloads:
  - signature: "constructor Create(WrapMode: TWrapMode = wmMirror); overload; virtual;"
    summary: "Creates sampler with specified wrap mode."
    parameters:
      - name: WrapMode
        type: TWrapMode
        description: "Coordinate wrap mode behavior. Default is wmMirror."
  - signature: "constructor Create(ColorGradient: TColor32Gradient); overload; virtual;"
    summary: "Creates sampler attached to a TColor32Gradient instance."
    parameters:
      - name: ColorGradient
        type: TColor32Gradient
        description: "Color gradient source."
---

## Description

Constructs a new `TCustomGradientSampler` instance.

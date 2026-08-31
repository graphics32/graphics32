---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomGradientSampler
entity: TCustomGradientSampler.Gradient
kind: Property
declaration: "property Gradient: TColor32Gradient read FGradient write SetGradient;"
summary: "The TColor32Gradient instance used for color stop evaluation."
---

## Description

Refers to the assigned [[TColor32Gradient]] instance. Setting `Gradient` attaches change event listeners so changes to gradient stops invalidate sampler cache.

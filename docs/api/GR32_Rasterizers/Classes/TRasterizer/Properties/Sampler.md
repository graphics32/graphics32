---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TRasterizer
entity: TRasterizer.Sampler
kind: Property
declaration: "property Sampler: TCustomSampler read FSampler write SetSampler;"
summary: "Specifies the TCustomSampler instance queried for color samples during rasterization."
---

## Description

`Sampler` references the [[TCustomSampler]] object used by this rasterizer to evaluate color values at continuous integer coordinates during [[Rasterize]].

Changing `Sampler` triggers a call to `Changed`, notifying attached persistent listeners.

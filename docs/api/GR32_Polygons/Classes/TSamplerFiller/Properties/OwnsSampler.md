---
layout: doc
docType: api
unit: GR32_Polygons
parent: TSamplerFiller
entity: TSamplerFiller.OwnsSampler
kind: Property
declaration: "property OwnsSampler: boolean read FOwnsSampler write FOwnsSampler;"
summary: "Indicates whether the filler owns and frees the sampler instance on destruction."
---

## Description

When `OwnsSampler` is `True`, `TSamplerFiller` automatically calls `Sampler.Free` when destroyed.

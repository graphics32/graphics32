---
layout: doc
docType: api
unit: GR32_Polygons
parent: TSamplerFiller
entity: TSamplerFiller.Create
kind: Constructor
summary: "Initializes a new TSamplerFiller with an optional sampler."
overloads:
  - signature: "constructor Create(Sampler: TCustomSampler = nil); reintroduce; overload; virtual;"
    summary: "Creates a TSamplerFiller bound to the specified TCustomSampler."
    parameters:
      - name: Sampler
        type: TCustomSampler
        description: "2D custom sampler instance."

  - signature: "constructor Create(Sampler: TCustomSampler; AOwnsSampler: boolean); overload;"
    summary: "Creates a TSamplerFiller bound to Sampler with explicit ownership semantics."
    parameters:
      - name: Sampler
        type: TCustomSampler
        description: "2D custom sampler instance."
      - name: AOwnsSampler
        type: boolean
        description: "True if filler should own and free the sampler on destruction."
---

## Description

`Create` instantiates a `TSamplerFiller` bound to the specified `Sampler`.

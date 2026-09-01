---
layout: doc
docType: api
unit: GR32_Resamplers
entity: CreateJitteredPattern
kind: Function
declaration: "function CreateJitteredPattern(TileWidth, TileHeight, SamplesX, SamplesY: Integer): TFixedSamplePattern;"
summary: "Generates a randomized jittered sample pattern grid for use with TPatternSampler."
parameters:
  - name: TileWidth, TileHeight
    type: Integer
    description: "Pattern tile grid dimensions."
  - name: SamplesX, SamplesY
    type: Integer
    description: "Number of sub-pixel sample points per grid tile axis."
---

## Description

`CreateJitteredPattern` allocates and populates a 2D sample offset pattern array with randomized sub-pixel jitter offsets for stochastic anti-aliasing in [[TPatternSampler]].

---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TDraftResampler
kind: Class
declaration: "TDraftResampler = class(TLinearResampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomResampler
  - TLinearResampler
  - TDraftResampler
summary: "Draft box-averaging resampler for fast downscaling previews."
---

## Description

`TDraftResampler` optimizes bitmap downscaling by averaging source pixel blocks, falling back to [[TLinearResampler]] when upscaling.

The downscaling result is better than nearest neighbor interpolation, but not quite as good as linear resampling.

::: tip
Downsampling with `TDraftResampler` is commonly used for fast generation of low quality thumbnail images.

It can also be used to provide a quick temporary preview result during interactive resampling.
:::

[members]

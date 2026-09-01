---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TNearestResampler
kind: Class
declaration: "TNearestResampler = class(TCustomResampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomResampler
  - TNearestResampler
summary: "High-speed nearest-neighbor bitmap resampler."
---

## Description

`TNearestResampler` implements nearest-neighbor pixel interpolation for fast image scaling operations where performance is critical.

[members]

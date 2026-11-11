---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TAdaptiveSuperSampler
entity: TAdaptiveSuperSampler.Tolerance
kind: Property
scope: Published
declaration: "property Tolerance: Integer read FTolerance write FTolerance;"
summary: "Specifies the color difference threshold that triggers recursive sub-pixel sampling."
---

## Description

`Tolerance` sets the color difference threshold that triggers recursive sub-pixel quadrant sampling. Lower values make the sampler more sensitive to minor color gradients, increasing overall sample density.

---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TNestedSampler
entity: TNestedSampler.Sampler
kind: Property
scope: Published
declaration: "property Sampler: TCustomSampler read FSampler write SetSampler;"
summary: "Refers to the nested target sampler instance wrapped by this sampler."
---

## Description

`Sampler` specifies the target [[TCustomSampler]] instance wrapped by this nested sampler. All coordinate transformations or filtering operations operate on samples retrieved from this inner sampler.

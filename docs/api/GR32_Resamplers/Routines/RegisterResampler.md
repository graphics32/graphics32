---
layout: doc
docType: api
unit: GR32_Resamplers
entity: RegisterResampler
kind: Function
declaration: "procedure RegisterResampler(ResamplerClass: TCustomResamplerClass);"
summary: "Registers resampler classes into the global resampler class registry."
parameters:
  - name: ResamplerClass
    type: TCustomResamplerClass
    description: "Class reference of the resampler to register."
---

## Description

`RegisterResampler` adds a resampler class to the global [[ResamplerList]] registry, allowing resamplers to be instantiated dynamically by class reference or string name.

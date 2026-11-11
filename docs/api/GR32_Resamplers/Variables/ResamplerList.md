---
layout: doc
docType: api
unit: GR32_Resamplers
entity: ResamplerList
kind: Variable
aliases: [TResamplerList]
declaration: |
  type
    TResamplerList = TCustomClassList<TCustomResamplerClass>;
  var
    ResamplerList: TResamplerList;
summary: "Global registry instance containing registered resampler classes."
---

## Description

`ResamplerList` is the global registry instance storing all registered [[TCustomResamplerClass]] types available for dynamic instantiation via [[RegisterResampler]].

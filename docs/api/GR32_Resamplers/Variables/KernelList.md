---
layout: doc
docType: api
unit: GR32_Resamplers
entity: KernelList
kind: Variable
aliases: [TKernelList]
declaration: |
  type
    TKernelList = TCustomClassList<TCustomKernelClass>;
  var
    KernelList: TKernelList;
summary: "Global registry instance containing registered resampler kernel classes."
---

## Description

`KernelList` is the global registry instance storing all registered [[TCustomKernelClass]] types available for dynamic instantiation via [[RegisterKernel]].

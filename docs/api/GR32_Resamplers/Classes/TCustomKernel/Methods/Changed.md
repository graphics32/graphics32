---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TCustomKernel
entity: TCustomKernel.Changed
kind: Method
scope: Public
declaration: "procedure Changed;"
summary: "Notifies observers that the kernel properties or filter parameters have changed."
---

## Description

`Changed` triggers notification to registered persistent observers (such as an attached [[TKernelResampler]]), forcing precomputed weight tables or filter caches to update.

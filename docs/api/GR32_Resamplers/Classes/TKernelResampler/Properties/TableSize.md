---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TKernelResampler
entity: TKernelResampler.TableSize
kind: Property
scope: Published
declaration: "property TableSize: Integer read FTableSize write SetTableSize;"
summary: "Resolution size of precomputed kernel weight lookup tables."
---

## Description

`TableSize` sets the number of discrete steps in the pre-calculated weight table when [[KernelMode]] is set to `kmTableNearest` or `kmTableLinear`.


---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TKernelSampler
entity: TKernelSampler.Kernel
kind: Property
scope: Published
declaration: |
  property Kernel: TIntegerMap read FKernel write SetKernel;
  property CenterX: Integer read FCenterX write FCenterX;
  property CenterY: Integer read FCenterY write FCenterY;
summary: "Specifies the 2D integer weight map and kernel origin coordinates."
---

## Description

`Kernel` specifies the 2D weight matrix map used for discrete convolution and morphological operations.

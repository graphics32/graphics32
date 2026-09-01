---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TCustomKernel
kind: Class
aliases: [TCustomKernelClass]
declaration: |
  type
    TCustomKernel = class(TPersistent)
      ...
    TCustomKernelClass = class of TCustomKernel;
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
summary: "Abstract base class for 1D spatial window filter functions."
---

## Description

`TCustomKernel` serves as the abstract base class for all spatial interpolation filter kernels in Graphics32. It defines the standard mathematical interface for evaluating 1D kernel weights ($f(x)$) and determining the kernel's spatial radius ($W$).

Descendant kernel classes implement specific mathematical functions such as Box, Linear, Cosine, Cubic, B-Spline, Gaussian, and Windowed Sinc filters.

[members]

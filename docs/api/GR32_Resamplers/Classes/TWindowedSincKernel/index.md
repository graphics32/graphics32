---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TWindowedSincKernel
kind: Class
summary: "Base class for Sinc spatial reconstruction kernels constrained by window functions."
declaration: "TWindowedSincKernel = class(TWindowedKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TWindowedSincKernel
---

## Description

`TWindowedSincKernel` is the base class for Sinc filters constrained by window functions $w(x)$:

$$
f(x) = \text{sinc}(x) \cdot w(x) = \frac{\sin(\pi x)}{\pi x} \cdot w(x) \quad (|x| < W)
$$

[members]

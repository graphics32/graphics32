---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TSwizzlingRasterizer
kind: Class
declaration: "TSwizzlingRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TSwizzlingRasterizer
summary: "Rasterizer that visits sample coordinates along space-filling fractal curves ('swizzling') to improve spatial locality."
---

## Description

`TSwizzlingRasterizer` samples pixels in a non-linear order determined by a space-filling fractal curve known as *swizzling* (related to Z-order / Morton curve traversal and the Sierpinski fractal curve).

Sampling along fractal swizzled space curves ensures that spatially adjacent destination pixels are sampled close together in time. This spatial coherency can significantly increase CPU data cache hit rates when sampling complex mathematical surfaces or textures with localized memory access patterns.

[members]

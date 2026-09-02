---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TRasterizer
kind: Class
abstract: true
aliases: [TRasterizerClass]
declaration: |
  type
    TRasterizer = class abstract(TThreadPersistent)
      ...
    TRasterizerClass = class of TRasterizer;
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
summary: "Abstract base class for bitmap-specific rasterizers that transfer spatial samples into target bitmap buffers."
---

## Description

`TRasterizer` serves as the abstract base class for all rasterization engines in Graphics32. Rasterizers iterate across 2D pixel coordinates within a destination bitmap ([[TCustomBitmap32]]), querying color values from an attached [[TCustomSampler]] and writing the resulting pixels using configurable combination and blend modes ([[TCombineInfo]]).

Key capabilities provided by `TRasterizer` include:
- Managing an attached [[TCustomSampler]] reference via the [[Sampler]] property.
- Handling drawing modes (`dmOpaque`, `dmBlend`, `dmTransparent`, `dmCustom`) and master alpha transparency.
- Managing sampling life cycle calls (`FSampler.PrepareSampling` and `FSampler.FinalizeSampling`).
- Providing flexible overloaded [[Rasterize]] entry points accepting custom target rectangles, combine info records, or source bitmap settings.

Derived classes override the protected `DoRasterize` method to implement specialized coordinate traversal and sampling order strategies (such as regular scanlines, swizzling fractal curves, progressive subsampling, or multi-threading).

[members]

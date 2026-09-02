---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TMultithreadedRegularRasterizer
kind: Class
declaration: "TMultithreadedRegularRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TMultithreadedRegularRasterizer
summary: "Cross-platform multi-threaded regular scanline rasterizer mapped to PPL or TThread implementation."
---

## Description

`TMultithreadedRegularRasterizer` provides a cross-platform multi-threaded rasterizer class.

At compile time, `TMultithreadedRegularRasterizer` automatically aliases to [[TParallelRegularRasterizer]] when Delphi PPL support is enabled (`USE_PPL`), or falls back to [[TThreadRegularRasterizer]] on legacy compilers.

[members]

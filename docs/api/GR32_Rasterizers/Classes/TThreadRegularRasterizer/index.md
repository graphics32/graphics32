---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TThreadRegularRasterizer
kind: Class
declaration: "TThreadRegularRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TThreadRegularRasterizer
summary: "Multi-threaded regular scanline rasterizer utilizing explicit TThread worker threads."
---

## Description

`TThreadRegularRasterizer` performs parallel scanline rasterization by spawning [[NumberOfProcessors]] dedicated worker threads (`TThread`). Each thread atomically claims scanlines via interlocked increments (`TInterlocked.Increment`) and renders assigned rows in parallel.

::: info Note
Spawning and destroying `TThread` instances on every call to `Rasterize` incurs thread creation overhead. For optimal performance on supported platforms, prefer [[TParallelRegularRasterizer]], [[TTaskRegularRasterizer]], or [[TMultithreadedRegularRasterizer]].
:::

[members]

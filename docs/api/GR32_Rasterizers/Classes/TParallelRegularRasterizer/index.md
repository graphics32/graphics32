---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TParallelRegularRasterizer
kind: Class
declaration: "TParallelRegularRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TParallelRegularRasterizer
summary: "Multi-threaded regular scanline rasterizer leveraging Delphi's Parallel Programming Library (TParallel.For)."
---

## Description

`TParallelRegularRasterizer` utilizes Delphi's Parallel Programming Library (PPL) `TParallel.For` loop to distribute scanlines evenly across the system thread pool.

By utilizing a persistent system thread pool, `TParallelRegularRasterizer` avoids thread creation overhead on repeated rendering calls.

::: info Note
First invocation may incur a minor performance penalty as the runtime thread pool warms up.
:::

[members]

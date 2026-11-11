---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TTaskRegularRasterizer
kind: Class
declaration: "TTaskRegularRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TTaskRegularRasterizer
summary: "Multi-threaded regular scanline rasterizer using partitioned task workers (TTask)."
---

## Description

`TTaskRegularRasterizer` uses asynchronous parallel tasks (`TTask.Run`) to process scanlines in parallel.

It partitions the scanline range into [[NumberOfProcessors]] contiguous scanline chunks, spawning worker tasks and waiting for completion via `TTask.WaitForAll`.

[members]

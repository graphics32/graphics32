---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TRegularRasterizer
kind: Class
declaration: "TRegularRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TRegularRasterizer
summary: "Standard rasterizer that evaluates one sample sequentially for each pixel in destination bitmap scanlines."
---

## Description

`TRegularRasterizer` is the standard single-threaded rasterizer in Graphics32. It iterates sequentially across pixel coordinates row by row from top to bottom, querying `GetSampleInt` for each $(X, Y)$ pixel position and assigning colors to destination bitmap scanlines.

When [[UpdateRowCount]] is greater than zero, `TRegularRasterizer` fires `Dst.Changed` area update notifications in batches after rendering every specified number of rows.

[members]

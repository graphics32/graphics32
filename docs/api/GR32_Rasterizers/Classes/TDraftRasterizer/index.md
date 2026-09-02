---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TDraftRasterizer
kind: Class
declaration: "TDraftRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TDraftRasterizer
summary: "Fast preview rasterizer that trades rendering quality for speed by sampling coarse pixel blocks."
---

## Description

`TDraftRasterizer` trades rendering precision for speed by pixelating the output buffer. Instead of querying [[Sampler]] for every destination pixel, it evaluates a single sample per block of size [[PixelSize]] $\times$ [[PixelSize]] and fills the entire block using `Dst.FillRect`.

`TDraftRasterizer` is suited for providing interactive live previews while dragging controls, manipulating complex procedural textures, or adjusting heavy resampling parameters.

[members]

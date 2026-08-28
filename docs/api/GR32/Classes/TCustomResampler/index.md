---
layout: doc
docType: api
unit: GR32
entity: TCustomResampler
kind: Class
declaration: |
  type
    TCustomResampler = class(TCustomSampler)
      ...
    TCustomResamplerClass = class of TCustomResampler;
aliases: [TCustomResampler]
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomResampler
summary: "Abstract base class for pixel resampling and filtering algorithms attached to `TBitmap32`."
---

## Description

`TCustomResampler` is the abstract base class for all bitmap resamplers in Graphics32. It inherits from `TCustomSampler` and adapts generic sub-pixel color sampling specifically for `TCustomBitmap32` instances.

Resamplers provide spatial filtering and interpolation algorithms (such as Nearest Neighbor, Linear, Cubic, or Kernel resamplers) used when stretching, transforming, or sampling bitmaps.

The `Bitmap` property associates the resampler with its source bitmap. The `PixelAccessMode` property controls out-of-bounds pixel sampling behavior (`pamUnsafe`, `pamSafe`, `pamWrap`, or `pamTransparentEdge`).

[members]

---
layout: doc
docType: api
unit: GR32
entity: TCustomResampler
kind: Class
declaration: "TCustomResampler = class(TCustomSampler)"
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

## Constructors

| Name | Description |
| --- | --- |
| [Create](Constructors/Create.md) | Initializes a new `TCustomResampler` instance, optionally attaching it to a source bitmap. |

## Methods

| Name | Description |
| --- | --- |
| [Resample](Methods/Resample.md) | Protected virtual abstract method that resamples a source bitmap area into a destination bitmap area. |

## Properties

| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [Bitmap](Properties/Bitmap.md) | `TCustomBitmap32` | Public | The source bitmap associated with this resampler. |
| [Width](Properties/Width.md) | `TFloat` | Public | Read-only effective kernel sampling width. |
| [PixelAccessMode](Properties/PixelAccessMode.md) | `TPixelAccessMode` | Published | Boundary handling mode (`pamUnsafe`, `pamSafe`, `pamWrap`, `pamTransparentEdge`). |

---
layout: doc
docType: api
unit: GR32
parent: TCustomResampler
entity: TCustomResampler.Bitmap
kind: Property
scope: Public
declaration: "property Bitmap: TCustomBitmap32 read FBitmap write FBitmap;"
summary: "Specifies or retrieves the source TCustomBitmap32 associated with this resampler."
---

## Description

`Bitmap` references the `TCustomBitmap32` instance whose pixel buffer is sampled by this resampler.

When constructing a resampler with `Create(ABitmap)`, `Bitmap` is assigned automatically. Changing `Bitmap` updates the target bitmap instance for subsequent sampling and resampling operations.

## Example

```pascal
Resampler.Bitmap := SourceBitmap;
```

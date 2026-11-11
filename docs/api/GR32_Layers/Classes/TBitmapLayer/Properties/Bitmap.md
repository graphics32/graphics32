---
layout: doc
docType: api
unit: GR32_Layers
parent: TBitmapLayer
entity: TBitmapLayer.Bitmap
kind: Property
scope: Public
declaration: "property Bitmap: TBitmap32 read GetBitmap write SetBitmap;"
summary: "Provides access to the owned bitmap instance."
---

## Description

`Bitmap` provides access to the internal `TBitmap32` owned by the layer.

::: Info
Setting a new bitmap **copies** the content of the new bitmap to the layer's internal bitmap. It does not transfer ownership of the bitmap to the layer.
:::
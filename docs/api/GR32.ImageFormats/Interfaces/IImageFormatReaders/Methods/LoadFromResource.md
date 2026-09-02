---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReaders
entity: IImageFormatReaders.LoadFromResource
kind: Method
scope: Public
declaration: "function LoadFromResource(ADest: TCustomBitmap32; AResourceType: TResourceType; AStream: TStream): Boolean;"
summary: "Loads an image resource stream matching the specified resource type."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: AResourceType
    type: TResourceType
    description: "Resource type indicator."
  - name: AStream
    type: TStream
    description: "Stream containing resource data."
---

## Description

`LoadFromResource` dispatches resource decoding to registered [[IImageFormatResourceReader]] implementations. Returns `True` if successful.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatResourceReader
entity: IImageFormatResourceReader.LoadFromResource
kind: Method
scope: Public
declaration: "function LoadFromResource(ADest: TCustomBitmap32; AResourceType: PChar; AStream: TStream): Boolean;"
summary: "Loads an image resource stream matching the specified resource type."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: AResourceType
    type: PChar
    description: "Resource type identifier."
  - name: AStream
    type: TStream
    description: "Stream wrapping the resource data."
---

## Description

`LoadFromResource` parses the resource stream using `AResourceType` and populates `ADest`. Returns `True` if successfully decoded.

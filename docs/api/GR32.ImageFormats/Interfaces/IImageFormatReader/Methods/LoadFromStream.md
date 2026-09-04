---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReader
entity: IImageFormatReader.LoadFromStream
kind: Method
scope: Public
declaration: "function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): Boolean;"
summary: "Decodes image data from a stream into the destination bitmap."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap to receive pixel buffer."
  - name: AStream
    type: TStream
    description: "Stream containing encoded image data."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`LoadFromStream` reads and decodes the image stream from `AStream` into `ADest`. Returns `True` if decoding succeeded.

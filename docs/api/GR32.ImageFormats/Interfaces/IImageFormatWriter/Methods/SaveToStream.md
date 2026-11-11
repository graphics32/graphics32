---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatWriter
entity: IImageFormatWriter.SaveToStream
kind: Method
scope: Public
declaration: "procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream);"
summary: "Encodes and writes pixel data from a bitmap into a stream."
parameters:
  - name: ASource
    type: TCustomBitmap32
    description: "Source bitmap containing pixel data to write."
  - name: AStream
    type: TStream
    description: "Destination stream."
---

## Description

`SaveToStream` encodes the contents of `ASource` into the format implemented by the writer and writes the output bytes to `AStream`.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReader
entity: IImageFormatReader.CanLoadFromStream
kind: Method
scope: Public
declaration: "function CanLoadFromStream(AStream: TStream): Boolean;"
summary: "Inspects stream contents or signatures to test whether the image format reader can decode it."
parameters:
  - name: AStream
    type: TStream
    description: "Stream containing image data."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`CanLoadFromStream` checks magic file signatures or header structure in `AStream` without modifying the permanent stream position. Returns `True` if the stream contains valid graphics data for this format.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReaders
entity: IImageFormatReaders.CanLoadFromStream
kind: Method
scope: Public
declaration: "function CanLoadFromStream(AStream: TStream): Boolean;"
summary: "Checks if any registered image reader can decode the contents of the given stream."
parameters:
  - name: AStream
    type: TStream
    description: "Stream to test."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`CanLoadFromStream` queries registered readers in priority order to determine if `AStream` contains supported image data.

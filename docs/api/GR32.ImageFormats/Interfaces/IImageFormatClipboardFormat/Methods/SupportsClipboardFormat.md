---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatClipboardFormat
entity: IImageFormatClipboardFormat.SupportsClipboardFormat
kind: Method
scope: Public
declaration: "function SupportsClipboardFormat(AFormat: TClipboardFormat): Boolean;"
summary: "Determines whether this handler supports a specific clipboard format identifier."
parameters:
  - name: AFormat
    type: TClipboardFormat
    description: "System clipboard format handle/ID to test."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`SupportsClipboardFormat` returns `True` if the image format handler knows how to decode clipboard data provided in format `AFormat`.

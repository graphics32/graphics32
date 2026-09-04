---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatClipboardFormats
entity: IImageFormatClipboardFormats.SupportsClipboardFormat
kind: Method
scope: Public
declaration: "function SupportsClipboardFormat(AFormat: TClipboardFormat): Boolean;"
summary: "Checks if any registered format handler supports the given clipboard format handle."
parameters:
  - name: AFormat
    type: TClipboardFormat
    description: "Clipboard format ID."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`SupportsClipboardFormat` iterates registered formats and returns `True` if any registered handler supports `AFormat`.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatClipboardFormat
entity: IImageFormatClipboardFormat.PasteFromClipboard
kind: Method
scope: Public
declaration: "function PasteFromClipboard(ADest: TCustomBitmap32): Boolean;"
summary: "Attempts to paste image data directly from the system clipboard into a bitmap."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap to receive clipboard pixel data."
---

## Description

`PasteFromClipboard` queries the system clipboard directly and decodes image data into `ADest`. Returns `True` if successful.

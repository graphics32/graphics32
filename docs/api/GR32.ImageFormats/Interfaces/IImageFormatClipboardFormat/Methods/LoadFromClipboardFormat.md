---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatClipboardFormat
entity: IImageFormatClipboardFormat.LoadFromClipboardFormat
kind: Method
scope: Public
declaration: "function LoadFromClipboardFormat(ADest: TCustomBitmap32; AFormat: TClipboardFormat; AData: THandle; APalette: THandle): Boolean;"
summary: "Loads pixel data from raw clipboard handles."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: AFormat
    type: TClipboardFormat
    description: "Clipboard format ID."
  - name: AData
    type: THandle
    description: "Global memory handle containing clipboard data."
  - name: APalette
    type: THandle
    description: "Optional palette handle."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`LoadFromClipboardFormat` extracts image data from global memory block handle `AData` using clipboard format `AFormat` and optional palette handle `APalette`. Returns `True` if successfully decoded into `ADest`.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatClipboardFormats
entity: IImageFormatClipboardFormats.PasteFromClipboard
kind: Method
scope: Public
declaration: "function PasteFromClipboard(ADest: TCustomBitmap32): Boolean;"
summary: "Pastes graphics data from the system clipboard into the destination bitmap."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`PasteFromClipboard` queries clipboard data in priority order across registered format handlers and populates `ADest`. Returns `True` if image pasting succeeded.

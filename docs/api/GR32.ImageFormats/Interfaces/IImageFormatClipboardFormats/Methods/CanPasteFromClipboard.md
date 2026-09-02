---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatClipboardFormats
entity: IImageFormatClipboardFormats.CanPasteFromClipboard
kind: Method
scope: Public
declaration: "function CanPasteFromClipboard: Boolean;"
summary: "Determines whether valid graphics data supported by any registered format is currently present on the system clipboard."
---

## Description

`CanPasteFromClipboard` inspects current system clipboard formats and returns `True` if any registered format handler can decode the available clipboard contents.

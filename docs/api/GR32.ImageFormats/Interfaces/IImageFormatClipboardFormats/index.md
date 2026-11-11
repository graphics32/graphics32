---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatClipboardFormats
kind: Interface
declaration: "IImageFormatClipboardFormats = interface"
summary: "Interface for clipboard operations aggregated across all registered image format adapters."
---

## Description

The `IImageFormatClipboardFormats` interface is implemented by the [[IImageFormatManager|image format manager]] to provide high-level clipboard dispatch methods across all registered image formats.

When the image format manager reads from the clipboard, it iterates all image formats that support the [[IImageFormatClipboardFormat]] interface; First it tries calling [[IImageFormatClipboardFormat.PasteFromClipboard|PasteFromClipboard]] on the image format and, if that isn't successful, it then iterates the available clipboard formats and call [[IImageFormatClipboardFormat.LoadFromClipboardFormat|LoadFromClipboardFormat]] on each in turn.<br>
If both of the above methods return `False`, it falls back to using the [[IImageFormatReader]] interface to try and read the data.

[members]

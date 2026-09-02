---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatAux
kind: Interface
declaration: "IImageFormatAux = interface"
summary: "Interface identifying auxiliary image formats that supplement primary formats."
---

## Description

An [[IImageFormatAdapter|image format adapter]] implements the `IImageFormatAux` interface to indicate that it is auxiliary (optional additional format). For example, when copying to the clipboard, PNG is an auxiliary format while `CF_DIBV5` is the primary format, and both should be placed on the clipboard.

[members]

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatAux
entity: IImageFormatAux.IsAuxFormat
kind: Method
scope: Public
declaration: "function IsAuxFormat(Source: TCustomBitmap32; Dest: TPersistent): Boolean;"
summary: "Determines whether the format acts as an auxiliary format for the given source and destination."
parameters:
  - name: Source
    type: TCustomBitmap32
    description: "Source bitmap."
  - name: Dest
    type: TPersistent
    description: "Target destination object."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`IsAuxFormat` returns `True` if this format should be processed as an auxiliary format alongside primary format handlers.

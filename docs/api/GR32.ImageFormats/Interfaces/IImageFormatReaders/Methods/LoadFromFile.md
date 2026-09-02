---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReaders
entity: IImageFormatReaders.LoadFromFile
kind: Method
scope: Public
declaration: "function LoadFromFile(ADest: TCustomBitmap32; const AFilename: String): Boolean;"
summary: "Loads image data from disk into the destination bitmap using registered file readers."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: AFilename
    type: String
    description: "File path."
---

## Description

`LoadFromFile` dispatches loading to registered [[IImageFormatFileReader]] implementations matching `AFilename`. Returns `True` if successfully loaded.

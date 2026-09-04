---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatFileReader
entity: IImageFormatFileReader.LoadFromFile
kind: Method
scope: Public
declaration: "function LoadFromFile(ADest: TCustomBitmap32; const AFilename: String): Boolean;"
summary: "Loads and decodes image data from a specified file on disk."
parameters:
  - name: ADest
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: AFilename
    type: String
    description: "Full file path to the image."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`LoadFromFile` reads the file specified by `AFilename` and loads pixel data into `ADest`. Returns `True` if successful.

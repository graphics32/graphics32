---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatWriters
entity: IImageFormatWriters.FindWriter
kind: Method
scope: Public
declaration: "function FindWriter(const AFileType: String): IImageFormatWriter;"
summary: "Locates a registered IImageFormatWriter matching the specified file extension."
parameters:
  - name: AFileType
    type: String
    description: "File extension string (e.g. 'png')."
---

## Description

`FindWriter` searches registered format writers for a writer registered with file extension `AFileType`. Returns the [[IImageFormatWriter]] instance, or `nil` if none was found.

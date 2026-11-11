---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatFileInfo
entity: IImageFormatFileInfo.ImageFormatFileTypes
kind: Method
scope: Public
declaration: "function ImageFormatFileTypes: TFileTypes;"
summary: "Returns an array of file extension strings associated with the image format."
returns:
  - type: TFileTypes
    description: "The calculated [[TFileTypes]] result."
---

## Description

`ImageFormatFileTypes` returns a dynamic array of strings ([[TFileTypes]]) containing the supported file extensions without leading dots (e.g. `['png']` or `['jpg', 'jpeg']`).

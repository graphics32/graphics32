---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatFileInfo
kind: Interface
declaration: "IImageFormatFileInfo = interface"
summary: "Interface providing file metadata for an image format."
---

## Description

`IImageFormatFileInfo` exposes human-readable descriptions and file extensions for an image format handler. This metadata is used by [[IImageFormatManager]] to construct open/save dialog file filters ([[BuildFileFilter]]) and match file extensions to reader/writer implementations.

[members]

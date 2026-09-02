---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatManager
entity: IImageFormatManager.RegisterImageFormat
kind: Method
scope: Public
declaration: "function RegisterImageFormat(const AImageFormat: IImageFormat; APriority: Integer = ImageFormatPriorityNormal): Integer;"
summary: "Registers an image format implementation with an optional priority level."
parameters:
  - name: AImageFormat
    type: IImageFormat
    description: "Image format interface implementation to register."
  - name: APriority
    type: Integer
    description: "Priority weight determining order in format resolution."
---

## Description

`RegisterImageFormat` registers `AImageFormat` into the central manager. Returns an integer registration handle that can be used to unregister the format later.

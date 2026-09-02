---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatManager
entity: IImageFormatManager.UnregisterImageFormat
kind: Method
summary: "Unregisters a previously registered image format instance or handle."
overloads:
  - signature: "procedure UnregisterImageFormat(const AImageFormat: IImageFormat); overload;"
    summary: "Unregisters an image format by interface instance."
    parameters:
      - name: AImageFormat
        type: IImageFormat
        description: "Format instance to unregister."
  - signature: "procedure UnregisterImageFormat(const AHandle: Integer); overload;"
    summary: "Unregisters an image format by registration handle."
    parameters:
      - name: AHandle
        type: Integer
        description: "Handle returned by RegisterImageFormat."
---

## Description

`UnregisterImageFormat` removes a registered image format from the manager.

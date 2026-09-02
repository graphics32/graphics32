---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatManager
entity: IImageFormatManager.ImageFormats
kind: Method
summary: "Returns an enumerable interface for iterating registered image formats."
overloads:
  - signature: "function ImageFormats: IImageFormats; overload;"
    summary: "Returns an enumerable container of all registered image formats."
  - signature: "function ImageFormats(Intf: TGUID): IImageFormats; overload;"
    summary: "Returns an enumerable container filtered to image formats supporting interface Intf."
    parameters:
      - name: Intf
        type: TGUID
        description: "Interface GUID filter (e.g. IImageFormatReader or IImageFormatWriter)."
---

## Description

`ImageFormats` provides an [[IImageFormats]] container for iterating registered formats.

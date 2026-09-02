---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReaders
entity: IImageFormatReaders.LoadFromStream
kind: Method
summary: "Decodes image data from a stream into a destination bitmap."
overloads:
  - signature: "function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): Boolean; overload;"
    summary: "Loads and decodes image stream contents into ADest by auto-detecting format signatures."
    parameters:
      - name: ADest
        type: TCustomBitmap32
        description: "Target bitmap."
      - name: AStream
        type: TStream
        description: "Input image stream."
  - signature: "function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream; const AFilename: String): Boolean; overload;"
    summary: "Loads and decodes image stream contents into ADest using AFilename's extension to narrow format discovery."
    parameters:
      - name: ADest
        type: TCustomBitmap32
        description: "Target bitmap."
      - name: AStream
        type: TStream
        description: "Input image stream."
      - name: AFilename
        type: String
        description: "Filename or extension context."
---

## Description

`LoadFromStream` attempts to decode graphics data from `AStream` into `ADest`.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatReaders
entity: IImageFormatReaders.FindReader
kind: Method
summary: "Finds a registered image format reader matching a file extension or stream signature."
overloads:
  - signature: "function FindReader(const AFileType: String): IImageFormatReader; overload;"
    summary: "Locates an IImageFormatReader registered for the given file extension."
    parameters:
      - name: AFileType
        type: String
        description: "File extension string (e.g. 'png')."
    returns:
      - type: IImageFormatReader
        description: "The calculated [[IImageFormatReader]] result."
  - signature: "function FindReader(AStream: TStream): IImageFormatReader; overload;"
    summary: "Inspects stream magic numbers/signatures to find a matching IImageFormatReader."
    parameters:
      - name: AStream
        type: TStream
        description: "Stream containing image data."

    returns:
      - type: IImageFormatReader
        description: "The calculated [[IImageFormatReader]] result."
---

## Description

`FindReader` searches registered format readers by file extension or by inspecting magic bytes in a stream. Returns the matching [[IImageFormatReader]], or `nil` if no matching reader is registered.

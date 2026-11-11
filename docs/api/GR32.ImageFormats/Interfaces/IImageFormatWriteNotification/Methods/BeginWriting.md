---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatWriteNotification
entity: IImageFormatWriteNotification.BeginWriting
kind: Method
scope: Public
declaration: "procedure BeginWriting(Source: TCustomBitmap32; Dest: TPersistent);"
summary: "Called immediately before assigning or exporting image data to a target."
parameters:
  - name: Source
    type: TCustomBitmap32
    description: "Source bitmap."
  - name: Dest
    type: TPersistent
    description: "Destination object."
---

## Description

`BeginWriting` notifies the image format adapter that an export/assignment transaction is starting.

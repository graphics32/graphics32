---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatWriteNotification
entity: IImageFormatWriteNotification.EndWriting
kind: Method
scope: Public
declaration: "procedure EndWriting(Source: TCustomBitmap32; Dest: TPersistent);"
summary: "Called immediately after assigning or exporting image data to a target."
parameters:
  - name: Source
    type: TCustomBitmap32
    description: "Source bitmap."
  - name: Dest
    type: TPersistent
    description: "Destination object."
---

## Description

`EndWriting` notifies the image format adapter that an export/assignment transaction has completed.

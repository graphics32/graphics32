---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatAdapter
entity: IImageFormatAdapter.CanAssignTo
kind: Method
scope: Public
declaration: "function CanAssignTo(Dest: TPersistent): Boolean;"
summary: "Determines whether this image format adapter can write pixel data from TCustomBitmap32 into the specified destination object."
parameters:
  - name: Dest
    type: TPersistent
    description: "Destination object to test."
---

## Description

`CanAssignTo` returns `True` if the adapter can export image data from a [[TCustomBitmap32]] into `Dest`.

---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatAdapter
entity: IImageFormatAdapter.CanAssignFrom
kind: Method
scope: Public
declaration: "function CanAssignFrom(Source: TPersistent): Boolean;"
summary: "Determines whether this image format adapter can copy or convert image data from the specified source object."
parameters:
  - name: Source
    type: TPersistent
    description: "Source graphic or object to test."
---

## Description

`CanAssignFrom` returns `True` if the adapter knows how to extract image data from `Source` into a [[TCustomBitmap32]] instance.

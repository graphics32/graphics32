---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatAdapter
entity: IImageFormatAdapter.AssignFrom
kind: Method
scope: Public
declaration: "function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): Boolean;"
summary: "Copies and converts image data from a source object into the destination bitmap."
parameters:
  - name: Dest
    type: TCustomBitmap32
    description: "Target bitmap to receive pixel data."
  - name: Source
    type: TPersistent
    description: "Source object containing graphic data."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`AssignFrom` converts pixel data from `Source` and populates `Dest`. Returns `True` if assignment succeeded, or `False` if the source format is unsupported.

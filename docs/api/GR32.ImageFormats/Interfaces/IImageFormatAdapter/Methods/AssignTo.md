---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatAdapter
entity: IImageFormatAdapter.AssignTo
kind: Method
scope: Public
declaration: "function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): Boolean;"
summary: "Exports pixel data from a source bitmap into the specified target object."
parameters:
  - name: Source
    type: TCustomBitmap32
    description: "Source bitmap containing pixel data."
  - name: Dest
    type: TPersistent
    description: "Destination target object."
---

## Description

`AssignTo` exports graphics data from `Source` into `Dest`. Returns `True` if successful, or `False` if assignment failed.

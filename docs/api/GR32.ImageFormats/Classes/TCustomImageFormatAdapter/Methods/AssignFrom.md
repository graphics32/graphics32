---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: TCustomImageFormatAdapter
entity: TCustomImageFormatAdapter.AssignFrom
kind: Method
scope: Protected
declaration: "function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): Boolean; virtual;"
summary: "Copies and converts image data from a source object into a destination bitmap."
parameters:
  - name: Dest
    type: TCustomBitmap32
    description: "Target bitmap."
  - name: Source
    type: TPersistent
    description: "Source graphic object."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`AssignFrom` checks `CanAssignFrom` and invokes `Dest.Assign(Source)`. Derived classes override this method to perform custom conversion logic.

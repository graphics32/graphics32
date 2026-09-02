---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: TCustomImageFormatAdapter
entity: TCustomImageFormatAdapter.AssignTo
kind: Method
scope: Protected
declaration: "function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): Boolean; virtual;"
summary: "Exports pixel data from a bitmap into a target destination object."
parameters:
  - name: Source
    type: TCustomBitmap32
    description: "Source bitmap."
  - name: Dest
    type: TPersistent
    description: "Target destination object."
---

## Description

`AssignTo` checks `CanAssignTo` and invokes `Dest.Assign(Source)`. Derived classes override this method to perform custom export logic.

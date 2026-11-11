---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: TCustomImageFormatAdapter
entity: TCustomImageFormatAdapter.CanAssignTo
kind: Method
scope: Protected
declaration: "function CanAssignTo(Dest: TPersistent): Boolean; virtual;"
summary: "Determines whether the image format adapter can export bitmap data to a destination object."
parameters:
  - name: Dest
    type: TPersistent
    description: "Destination object."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`CanAssignTo` returns `False` by default in `TCustomImageFormatAdapter`. Derived classes override this method to specify supported target types.

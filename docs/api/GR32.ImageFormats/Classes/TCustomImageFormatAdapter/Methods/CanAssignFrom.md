---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: TCustomImageFormatAdapter
entity: TCustomImageFormatAdapter.CanAssignFrom
kind: Method
scope: Protected
declaration: "function CanAssignFrom(Source: TPersistent): Boolean; virtual;"
summary: "Determines whether the image format adapter can copy or convert image data from a source object."
parameters:
  - name: Source
    type: TPersistent
    description: "Source object to test."
returns:
  - type: Boolean
    description: "Returns `True` if successful or supported; otherwise `False`."
---

## Description

`CanAssignFrom` returns `False` by default in `TCustomImageFormatAdapter`. Derived classes override this method to specify supported source types.

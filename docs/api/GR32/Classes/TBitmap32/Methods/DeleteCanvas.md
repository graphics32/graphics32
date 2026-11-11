---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.DeleteCanvas
kind: Method
scope: Public
declaration: "procedure DeleteCanvas;"
summary: "Frees and releases the internal VCL/LCL TCanvas instance allocated for this bitmap."
seealso:
  - "[[Canvas]]"
  - "[[CanvasAllocated]]"
---

## Description

`DeleteCanvas` releases the internal `TCanvas` instance associated with this bitmap if `ICanvasSupport` is implemented by the backend. Subsequent calls to `Canvas` will re-create a canvas instance if needed.

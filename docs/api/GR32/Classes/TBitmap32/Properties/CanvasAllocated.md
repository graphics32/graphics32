---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.CanvasAllocated
kind: Property
scope: Public
declaration: "property CanvasAllocated: boolean read GetCanvasAllocated;"
summary: "Indicates whether a VCL/LCL TCanvas instance is currently allocated for this bitmap."
seealso:
  - "[[Canvas]]"
  - "[[DeleteCanvas]]"
---

## Description

`CanvasAllocated` returns `True` if a `TCanvas` instance has been created and bound to this bitmap backend via `ICanvasSupport`.

Use `CanvasAllocated` to check if a canvas is actively allocated before performing canvas-related cleanup or operations.

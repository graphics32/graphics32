---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.Create
kind: Constructor
scope: Public
declaration: "constructor Create(AOwner: TComponent); override;"
summary: "Creates and initializes a TCustomPaintBox32 instance."
parameters:
  - name: AOwner
    type: TComponent
    description: "Component owner managing the control lifecycle."
---

## Description

`Create` instantiates a new [[TCustomPaintBox32]] control, initializing internal double-buffered `Buffer` surface (`TBitmap32`), invalidation tracking lists, and default repaint options.

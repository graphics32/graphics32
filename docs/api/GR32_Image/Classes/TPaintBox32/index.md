---
layout: doc
docType: api
unit: GR32_Image
entity: TPaintBox32
kind: Class
declaration: "TPaintBox32 = class(TCustomPaintBox32);"
inheritance:
  - TGraphics32ControlBaseClass
  - TCustomPaintBox32
  - TPaintBox32
summary: "Lightweight paint box control for double-buffered custom TBitmap32 rendering."
---

## Description

`TPaintBox32` is a visual paint box component derived from [[TCustomPaintBox32]]. Applications handle the `OnPaintBuffer` event to render directly onto the control's double-buffered `Buffer` surface.

[members]

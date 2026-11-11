---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Canvas
kind: Property
scope: Public
declaration: "property Canvas: TCanvas read GetCanvas;"
summary: "Provides a VCL/LCL TCanvas instance bound to the bitmap's GDI device context (HDC)."
---

## Description

`Canvas` provides access to standard VCL/LCL canvas drawing methods (GDI text rendering, shapes, lines) operating on the bitmap surface.

## Example

```pascal
Bitmap.Canvas.TextOut(10, 10, 'Hello Graphics32');
```

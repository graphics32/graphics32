---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Pixel
kind: Property
scope: Public
declaration: "property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;"
summary: "Default indexed property for reading and writing 32-bit ARGB pixel values at integer coordinates."
---

## Description

`Pixel` provides direct indexed pixel access `[X, Y]`. Coordinates outside bitmap bounds perform direct unclipped memory access.

## Example

```pascal
Color := Bitmap[10, 20];
Bitmap[10, 20] := clRed32;
```

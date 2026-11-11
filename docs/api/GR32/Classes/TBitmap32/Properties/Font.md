---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Font
kind: Property
scope: Public
declaration: "property Font: TFont read GetFont write SetFont;"
summary: "Specifies or retrieves the VCL/LCL font used when rendering text onto the Canvas."
---

## Description

`Font` manages text formatting parameters (font name, size, style, color) for `Canvas.TextOut` operations.

## Example

```pascal
Bitmap.Font.Name := 'Arial';
Bitmap.Font.Size := 16;
```

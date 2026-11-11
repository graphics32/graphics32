---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LoadFromFile
kind: Method
scope: Public
declaration: "procedure LoadFromFile(const FileName: string); virtual;"
summary: "Loads bitmap image data from a specified file path."
parameters:
  - name: FileName
    type: string
    description: "Full file path of the image file to load."
---

## Description

`LoadFromFile` opens and reads image data from `FileName`.

## Example

```pascal
Bitmap.LoadFromFile('C:\Images\Sample.png');
```

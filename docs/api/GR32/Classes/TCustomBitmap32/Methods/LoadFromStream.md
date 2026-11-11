---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LoadFromStream
kind: Method
scope: Public
declaration: "procedure LoadFromStream(Stream: TStream); virtual;"
summary: "Loads bitmap image data from a stream."
parameters:
  - name: Stream
    type: TStream
    description: "Stream containing encoded bitmap image data."
---

## Description

`LoadFromStream` reads image data from `Stream` using registered image format handlers or standard BMP decoder routines.

## Example

```pascal
Bitmap.LoadFromStream(Stream);
```

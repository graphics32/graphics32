---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Delete
kind: Method
scope: Public
declaration: "procedure Delete; override;"
summary: "Resets bitmap dimensions to zero and deallocates pixel buffer memory."
---

## Description

`Delete` sets `Width` and `Height` to zero and releases allocated pixel buffer memory.

## Example

```pascal
Bitmap.Delete;
```

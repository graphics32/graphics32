---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Empty
kind: Method
scope: Public
declaration: "function Empty: Boolean; override;"
summary: "Returns True if either Width or Height of the bitmap is zero."
returns:
  - type: Boolean
    description: "Returns `True` if the bitmap has zero area or an unallocated pixel buffer; otherwise `False`."
---

## Description

`Empty` returns `True` if the bitmap has zero pixel width or height.

## Example

```pascal
if not Bitmap.Empty then
  Bitmap.Clear;
```

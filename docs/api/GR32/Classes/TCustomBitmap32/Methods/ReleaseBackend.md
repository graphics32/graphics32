---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.ReleaseBackend
kind: Method
scope: Public
declaration: "function ReleaseBackend: TCustomBackend;"
summary: "Detaches and returns the active TCustomBackend instance without freeing it."
returns:
  - type: TCustomBackend
    description: "The previous [[TCustomBackend]] instance detached from the bitmap."
---

## Description

`ReleaseBackend` detaches the active backend from the bitmap and returns it to the caller, allowing transfer of ownership.

## Example

```pascal
Backend := Bitmap.ReleaseBackend;
```

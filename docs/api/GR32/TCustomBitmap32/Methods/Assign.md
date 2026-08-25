---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Assign
kind: Method
scope: Public
declaration: "procedure Assign(Source: TPersistent); override;"
summary: "Copies pixel data, dimensions, and drawing properties from a source object or persistent bitmap."
parameters:
  - name: Source
    type: TPersistent
    description: "Source object (typically another TCustomBitmap32 or TGraphic) to copy from."
---

## Description

`Assign` copies pixel data, dimensions, drawing parameters, and active settings from `Source` into this bitmap.

If `Source` is another `TCustomBitmap32`, `Assign` resizes this bitmap, copies pixel buffer memory, and copies drawing properties (`DrawMode`, `CombineMode`, `MasterAlpha`, `OuterColor`, `WrapMode`, `ClipRect`).

## Example

```pascal
DstBitmap.Assign(SrcBitmap);
```

---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.GetBitmapSize
kind: Method
declaration: "function GetBitmapSize: TSize; virtual;"
summary: "Returns scaled dimensions of bitmap in pixels."
returns:
  - type: TSize
    description: "Width (cx) and height (cy) of scaled bitmap."
seealso:
  - "[[GetBitmapRect]]"
---

## Description

`GetBitmapSize` calculates the rendered pixel width and height of `Bitmap` based on current `ScaleMode`.

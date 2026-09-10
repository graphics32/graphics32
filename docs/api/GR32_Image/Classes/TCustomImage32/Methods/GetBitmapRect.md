---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.GetBitmapRect
kind: Method
declaration: "function GetBitmapRect: TRect; virtual;"
summary: "Calculates bounding rectangle of scaled bitmap in control viewport coordinates."
returns:
  - type: TRect
    description: "Bounding rectangle of scaled bitmap in viewport space."
seealso:
  - "[[GetBitmapSize]]"
---

## Description

`GetBitmapRect` computes the viewport destination rectangle of the bitmap, taking into account `ScaleMode`, `BitmapAlign`, `ScaleX`, `ScaleY`, `OffsetHorz`, and `OffsetVert`.

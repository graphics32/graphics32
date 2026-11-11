---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.BitmapAlign
kind: Property
aliases: [TBitmapAlign, baTopLeft, baCenter, baTile, baCustom]
declaration: |
  type
    TBitmapAlign = (baTopLeft, baCenter, baTile, baCustom);

  property BitmapAlign: TBitmapAlign read FBitmapAlign write SetBitmapAlign;
summary: "Specifies bitmap positioning and alignment behavior within the viewport."
seealso:
  - "[[ScaleMode]]"
---

## Description

`BitmapAlign` specifies how the bitmap is positioned within the control viewport when [[ScaleMode]] is set to `smNormal` or `smScale`.

| Value | Description |
| --- | --- |
| `baTopLeft` | Aligns the top-left corner of the bitmap with the top-left corner of the viewport control. |
| `baCenter` | Centers the bitmap horizontally and vertically within the viewport control. |
| `baTile` | Tiles the bitmap repeatedly across the entire client area of the control. |
| `baCustom` | Enables custom positioning using [[OffsetHorz]] and [[OffsetVert]] properties. |

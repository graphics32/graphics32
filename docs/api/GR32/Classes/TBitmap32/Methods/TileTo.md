---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.TileTo
kind: Method
scope: Public
declaration: "procedure TileTo(hDst: HDC; const DstRect, SrcRect: TRect; MaxTileSize: integer = 1024);"
summary: "Tiles a sub-rectangle of this bitmap repeatedly onto a target GDI device context (HDC)."
parameters:
  - name: hDst
    type: HDC
    description: "Target GDI device context handle."
  - name: DstRect
    type: TRect
    description: "Target destination rectangle to fill with repeating tiles."
  - name: SrcRect
    type: TRect
    description: "Source sub-rectangle of this bitmap used as the tile unit."
  - name: MaxTileSize
    type: Integer
    description: "Maximum tile chunk size limit (default 1024)."
---

## Description

`TileTo` tiles the source sub-rectangle `SrcRect` repeatedly across `DstRect` on the target GDI device context `hDst`.

## Example

```pascal
Bitmap.TileTo(Canvas.Handle, Canvas.ClipRect, Bitmap.BoundsRect);
```

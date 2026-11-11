---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Draw
kind: Method
scope: Public
summary: "Draws a source bitmap or sub-rectangle onto this bitmap using current DrawMode and CombineMode."
overloads:
  - signature: "procedure Draw(DstX, DstY: Integer; Src: TCustomBitmap32); overload;"
    summary: "Draws the entire source bitmap at top-left pixel position (DstX, DstY)."
    parameters:
      - name: DstX, DstY
        type: Integer
        description: "Top-left destination coordinate on this bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap to draw."

  - signature: "procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TCustomBitmap32); overload;"
    summary: "Draws a source sub-rectangle onto this bitmap at top-left position (DstX, DstY)."
    parameters:
      - name: DstX, DstY
        type: Integer
        description: "Top-left destination coordinate on this bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle to copy."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."

  - signature: "procedure Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32); overload;"
    summary: "Stretches and blends a sub-rectangle from the source bitmap into a destination rectangle."
    parameters:
      - name: DstRect
        type: TRect
        description: "Target destination rectangle on this bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle on the source bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
---

## Description

`Draw` blits or resamples pixel data from `Src` onto this bitmap surface.

If active resamplers are set on `Src`, stretch drawing operations use the specified interpolation algorithm.

## Example

```pascal
DstBitmap.Draw(0, 0, SrcBitmap);
```

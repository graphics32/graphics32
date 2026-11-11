---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Draw
kind: Method
scope: Public
summary: "Draws a source bitmap, sub-rectangle, or GDI device context (HDC) onto this bitmap using current DrawMode and CombineMode."
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
    summary: "Draws a sub-rectangle from the source bitmap at top-left pixel position (DstX, DstY)."
    parameters:
      - name: DstX, DstY
        type: Integer
        description: "Top-left destination coordinate on this bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle on the source bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap to copy or blend pixels from."

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
        description: "Source bitmap to copy or blend pixels from."

  - signature: "procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;"
    summary: "Copies a sub-rectangle from an external GDI device context (HDC) into a destination rectangle on this bitmap."
    parameters:
      - name: DstRect
        type: TRect
        description: "Destination rectangle on this bitmap."
      - name: SrcRect
        type: TRect
        description: "Source rectangle in the external GDI device context."
      - name: hSrc
        type: HDC
        description: "Handle to the source GDI device context."
---

## Description

`Draw` blits pixel data onto this bitmap. In addition to standard bitmap-to-bitmap blitting, `TBitmap32` provides an overload to copy pixels directly from an external GDI device context handle (`hSrc`).

## Example

```pascal
// Copy screen area directly from Windows desktop DC
Bitmap.Draw(Bitmap.BoundsRect, Rect(0, 0, 100, 100), GetDC(0));
```

---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.DrawTo
kind: Method
scope: Public
summary: "Draws this bitmap or a sub-rectangle onto a target destination bitmap."
overloads:
  - signature: "procedure DrawTo(Dst: TCustomBitmap32); overload;"
    summary: "Draws this entire bitmap onto the destination bitmap at (0, 0)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target destination bitmap."

  - signature: "procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer); overload;"
    summary: "Draws this entire bitmap onto the destination bitmap at (DstX, DstY)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination coordinates."

  - signature: "procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer; const SrcRect: TRect); overload;"
    summary: "Draws a sub-rectangle of this bitmap onto the destination bitmap at (DstX, DstY)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination coordinates."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle."

  - signature: "procedure DrawTo(Dst: TCustomBitmap32; const DstRect: TRect); overload;"
    summary: "Stretches this entire bitmap into the destination rectangle of Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target destination bitmap."
      - name: DstRect
        type: TRect
        description: "Target destination rectangle."

  - signature: "procedure DrawTo(Dst: TCustomBitmap32; const DstRect, SrcRect: TRect); overload;"
    summary: "Stretches a sub-rectangle of this bitmap into a destination rectangle on Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target destination bitmap."
      - name: DstRect
        type: TRect
        description: "Target destination rectangle."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle."
---

## Description

`DrawTo` draws this bitmap onto `Dst`.

## Example

```pascal
SrcBitmap.DrawTo(DstBitmap, 10, 10);
```

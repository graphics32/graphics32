---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.DrawTo
kind: Method
scope: Public
summary: "Draws this bitmap onto a destination bitmap or GDI device context (HDC)."
overloads:
  - signature: "procedure DrawTo(Dst: TCustomBitmap32); overload;"
    summary: "Draws this entire bitmap onto destination bitmap at (0, 0)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."

  - signature: "procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer); overload;"
    summary: "Draws this entire bitmap onto destination bitmap at (DstX, DstY)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination coordinates."

  - signature: "procedure DrawTo(hDst: HDC; DstX: Integer = 0; DstY: Integer = 0); overload;"
    summary: "Renders this bitmap onto a target GDI device context (HDC) at top-left position (DstX, DstY)."
    parameters:
      - name: hDst
        type: HDC
        description: "Target GDI device context handle."
      - name: DstX, DstY
        type: Integer
        description: "Top-left destination coordinates."

  - signature: "procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;"
    summary: "Stretches a sub-rectangle of this bitmap onto a target GDI device context (HDC) rectangle."
    parameters:
      - name: hDst
        type: HDC
        description: "Target GDI device context handle."
      - name: DstRect
        type: TRect
        description: "Target destination rectangle on HDC."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle on this bitmap."
---

## Description

`DrawTo` renders this bitmap surface directly onto another bitmap or an external OS GDI device context (`hDst`).

## Example

```pascal
// Paint bitmap onto Form's Canvas handle in OnPaint event
Bitmap.DrawTo(Form.Canvas.Handle, 0, 0);
```

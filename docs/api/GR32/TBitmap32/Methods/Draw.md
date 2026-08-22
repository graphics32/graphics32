---
layout: doc
docType: api
unit: GR32
entity: TBitmap32.Draw
kind: Method
declaration: |
  procedure Draw(DstX, DstY: Integer; Src: TCustomBitmap32); overload;
  procedure Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32); overload;
summary: "Draws a source bitmap or sub-rectangle onto this bitmap using current DrawMode and CombineMode."
parameters:
  - name: DstX, DstY
    type: Integer
    description: "Top-left destination coordinate on this bitmap."
  - name: DstRect
    type: TRect
    description: "Target destination rectangle on this bitmap."
  - name: SrcRect
    type: TRect
    description: "Source rectangle on the source bitmap."
  - name: Src
    type: TCustomBitmap32
    description: "Source bitmap to copy or blend pixels from."
---

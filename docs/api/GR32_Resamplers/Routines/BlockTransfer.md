---
layout: doc
docType: api
unit: GR32_Resamplers
entity: BlockTransfer
kind: Function
summary: "Performs unscaled block pixel transfer between source and destination bitmaps."
overloads:
  - signature: "procedure BlockTransfer(Dst: TCustomBitmap32; DstX: Integer; DstY: Integer; DstClip: TRect; Src: TCustomBitmap32; SrcRect: TRect; CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);"
    summary: "Copies or blends an integer-aligned sub-rectangle from Src into Dst at integer position (DstX, DstY)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination top-left corner coordinates."
      - name: DstClip
        type: TRect
        description: "Clipping rectangle for the transfer operation."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: SrcRect
        type: TRect
        description: "Sub-rectangle of source bitmap to transfer."
      - name: CombineOp
        type: TDrawMode
        description: "Pixel drawing/combining mode."
      - name: CombineCallBack
        type: TPixelCombineEvent
        description: "Optional custom pixel combining callback."

  - signature: "procedure BlockTransferX(Dst: TCustomBitmap32; DstX, DstY: TFixed; Src: TCustomBitmap32; SrcRect: TRect; CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);"
    summary: "Copies or blends an integer sub-rectangle from Src into Dst at fixed-point sub-pixel position (DstX, DstY) using bilinear anti-aliasing."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: TFixed
        description: "Fixed-point destination top-left coordinates."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle."
      - name: CombineOp
        type: TDrawMode
        description: "Pixel combining mode."
      - name: CombineCallBack
        type: TPixelCombineEvent
        description: "Optional custom pixel combine callback."
---

## Description

`BlockTransfer` performs high-speed direct unscaled pixel transfers from a source bitmap to a destination bitmap with optional clipping and pixel blending.
`BlockTransferX` allows fixed-point fractional offsets for sub-pixel accuracy.

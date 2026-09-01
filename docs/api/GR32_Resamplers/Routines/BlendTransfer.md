---
layout: doc
docType: api
unit: GR32_Resamplers
entity: BlendTransfer
kind: Function
summary: "Blends pixels between two source bitmaps into a destination bitmap using a custom blend register function."
overloads:
  - signature: "procedure BlendTransfer(Dst: TCustomBitmap32; DstX, DstY: Integer; DstClip: TRect; SrcF: TCustomBitmap32; SrcRectF: TRect; SrcB: TCustomBitmap32; SrcRectB: TRect; BlendCallback: TBlendReg); overload;"
    summary: "Blends foreground bitmap SrcF and background bitmap SrcB into Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination offset coordinates."
      - name: DstClip
        type: TRect
        description: "Destination clip rectangle."
      - name: SrcF
        type: TCustomBitmap32
        description: "Foreground source bitmap."
      - name: SrcRectF
        type: TRect
        description: "Foreground source rectangle."
      - name: SrcB
        type: TCustomBitmap32
        description: "Background source bitmap."
      - name: SrcRectB
        type: TRect
        description: "Background source rectangle."
      - name: BlendCallback
        type: TBlendReg
        description: "Low-level register blend procedure."

  - signature: "procedure BlendTransfer(Dst: TCustomBitmap32; DstX, DstY: Integer; DstClip: TRect; SrcF: TCustomBitmap32; SrcRectF: TRect; SrcB: TCustomBitmap32; SrcRectB: TRect; BlendCallback: TBlendRegEx; MasterAlpha: Integer); overload;"
    summary: "Blends foreground and background bitmaps into Dst with a master alpha level."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination position."
      - name: DstClip
        type: TRect
        description: "Clip rectangle."
      - name: SrcF
        type: TCustomBitmap32
        description: "Foreground source bitmap."
      - name: SrcRectF
        type: TRect
        description: "Foreground rect."
      - name: SrcB
        type: TCustomBitmap32
        description: "Background source bitmap."
      - name: SrcRectB
        type: TRect
        description: "Background rect."
      - name: BlendCallback
        type: TBlendRegEx
        description: "Extended blend function."
      - name: MasterAlpha
        type: Integer
        description: "Master alpha weight [0..255]."
---

## Description

`BlendTransfer` combines corresponding pixels from two source bitmaps (`SrcF` and `SrcB`) into `Dst` using high-performance register blend routines.

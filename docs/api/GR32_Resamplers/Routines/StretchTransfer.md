---
layout: doc
docType: api
unit: GR32_Resamplers
entity: StretchTransfer
kind: Function
declaration: "procedure StretchTransfer(Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect; Src: TCustomBitmap32; SrcRect: TRect; Resampler: TCustomResampler; CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);"
summary: "Stretches and resamples a source bitmap rectangle onto a destination bitmap rectangle using a specified resampler."
parameters:
  - name: Dst
    type: TCustomBitmap32
    description: "Target destination bitmap."
  - name: DstRect
    type: TRect
    description: "Destination bounding rectangle."
  - name: DstClip
    type: TRect
    description: "Clipping rectangle on destination bitmap."
  - name: Src
    type: TCustomBitmap32
    description: "Source bitmap to resample."
  - name: SrcRect
    type: TRect
    description: "Source rectangle bounds."
  - name: Resampler
    type: TCustomResampler
    description: "Resampler instance used to perform pixel reconstruction."
  - name: CombineOp
    type: TDrawMode
    description: "Drawing mode (opaque, blend, transparent, custom)."
  - name: CombineCallBack
    type: TPixelCombineEvent
    description: "Custom pixel combining callback when CombineOp is dmCustom."
---

## Description

`StretchTransfer` rescales and blends `SrcRect` from `Src` into `DstRect` of `Dst` using the spatial filtering algorithm provided by `Resampler`.

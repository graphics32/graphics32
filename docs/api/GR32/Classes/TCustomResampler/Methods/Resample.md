---
layout: doc
docType: api
unit: GR32
parent: TCustomResampler
entity: TCustomResampler.Resample
kind: Method
scope: Protected
declaration: "procedure Resample(Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect; Src: TCustomBitmap32; SrcRect: TRect; CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); virtual; abstract;"
summary: "Resamples a source bitmap area into a destination bitmap area using specified draw mode and clipping rectangle."
parameters:
  - name: Dst
    type: TCustomBitmap32
    description: "Destination bitmap receiving resampled output."
  - name: DstRect
    type: TRect
    description: "Target rectangle region in the destination bitmap."
  - name: DstClip
    type: TRect
    description: "Clipping rectangle restricting writing in the destination bitmap."
  - name: Src
    type: TCustomBitmap32
    description: "Source bitmap containing pixel data to resample."
  - name: SrcRect
    type: TRect
    description: "Source rectangle region to resample from."
  - name: CombineOp
    type: TDrawMode
    description: "Pixel draw/combine mode (e.g. dmOpaque, dmBlend)."
  - name: CombineCallBack
    type: TPixelCombineEvent
    description: "Optional custom pixel combination callback procedure."
---

## Description

`Resample` is a protected virtual abstract method implemented by derived resampler classes (e.g. `TNearestResampler`, `TLinearResampler`, `TKernelResampler`).

It performs high-performance block stretch-drawing and resampling from `Src` rectangle `SrcRect` into `Dst` rectangle `DstRect`, subject to destination clipping rectangle `DstClip` and pixel draw mode `CombineOp`.

This method is called internally by `TCustomBitmap32.Draw` and `TCustomBitmap32.StretchDraw` when resamplers are active.

## Remarks

Because `Resample` is an abstract method, it must be overridden by concrete resampler subclasses.

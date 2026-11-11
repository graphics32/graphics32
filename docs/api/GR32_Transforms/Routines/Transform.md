---
layout: doc
docType: api
unit: GR32_Transforms
entity: Transform
kind: Function
summary: "Resamples a source bitmap into a destination bitmap using a spatial coordinate transformation."
overloads:
  - signature: "procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Reverse: boolean = True); overload;"
    summary: "Resamples Src into Dst across Dst's full bounds."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: Transformation
        type: TTransformation
        description: "Transformation object."
      - name: Reverse
        type: boolean
        description: "If True (default), uses inverse mapping (ReverseTransform)."

  - signature: "procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; const DstClip: TRect; Reverse: boolean = True); overload;"
    summary: "Resamples Src into Dst clipped to DstClip."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: Transformation
        type: TTransformation
        description: "Transformation object."
      - name: DstClip
        type: TRect
        description: "Destination clipping rectangle."
      - name: Reverse
        type: boolean
        description: "If True (default), uses inverse mapping."

  - signature: "procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Rasterizer: TRasterizer; Reverse: boolean = True); overload;"
    summary: "Resamples Src into Dst using a specific rasterizer instance."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: Transformation
        type: TTransformation
        description: "Transformation object."
      - name: Rasterizer
        type: TRasterizer
        description: "Custom rasterizer to perform scanline sampling."
      - name: Reverse
        type: boolean
        description: "If True (default), uses inverse mapping."

  - signature: "procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Rasterizer: TRasterizer; const DstClip: TRect; Reverse: boolean = True); overload;"
    summary: "Resamples Src into Dst using a specific rasterizer and destination clipping rectangle."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: Transformation
        type: TTransformation
        description: "Transformation object."
      - name: Rasterizer
        type: TRasterizer
        description: "Custom rasterizer."
      - name: DstClip
        type: TRect
        description: "Destination clipping rectangle."
      - name: Reverse
        type: boolean
        description: "If True (default), uses inverse mapping."
---

## Description

`Transform` resamples pixel data from a source bitmap `Src` into `Dst` according to a geometric mapping defined by `Transformation`. Standard inverse sampling (`Reverse = True`) prevents pixel gaps and aliasing artifacts during warp operations.

---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TRasterizer
entity: TRasterizer.Rasterize
kind: Method
summary: "Executes sampling across the specified destination bitmap area and writes sampled colors into pixel memory."
overloads:
  - signature: "procedure Rasterize(Dst: TCustomBitmap32); overload;"
    summary: "Rasterizes the entire bounds rectangle of the destination bitmap."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target bitmap into which sampled pixel colors are rendered."

  - signature: "procedure Rasterize(Dst: TCustomBitmap32; const DstRect: TRect); overload;"
    summary: "Rasterizes a specific sub-rectangle of the destination bitmap."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target bitmap into which sampled pixel colors are rendered."
      - name: DstRect
        type: TRect
        description: "Destination rectangle bounding the pixel area to sample and render."

  - signature: "procedure Rasterize(Dst: TCustomBitmap32; const DstRect: TRect; const CombineInfo: TCombineInfo); overload;"
    summary: "Rasterizes a sub-rectangle using explicitly specified pixel combination settings."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target bitmap into which sampled pixel colors are rendered."
      - name: DstRect
        type: TRect
        description: "Destination rectangle bounding the pixel area to sample and render."
      - name: CombineInfo
        type: TCombineInfo
        description: "Record specifying draw mode, combine mode, master alpha, and custom callbacks."

  - signature: "procedure Rasterize(Dst: TCustomBitmap32; const DstRect: TRect; Src: TCustomBitmap32); overload;"
    summary: "Rasterizes a sub-rectangle using pixel combination settings extracted from a source bitmap."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Target bitmap into which sampled pixel colors are rendered."
      - name: DstRect
        type: TRect
        description: "Destination rectangle bounding the pixel area to sample and render."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap whose drawing modes and alpha settings are used during rendering."
---

## Description

`Rasterize` initiates rendering of the attached [[Sampler]] onto the destination bitmap `Dst`.

Before sampling, `Rasterize` calls `FSampler.PrepareSampling` to allow the sampler to initialize lookup structures or pre-calculate transforms. It then computes the clipped intersection rectangle between `DstRect`, `Dst.BoundsRect`, and `FSampler.GetSampleBounds` before delegating pixel generation to `DoRasterize`. After rendering completes, `FSampler.FinalizeSampling` is invoked.

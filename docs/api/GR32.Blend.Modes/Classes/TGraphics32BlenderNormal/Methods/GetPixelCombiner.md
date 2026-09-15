---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TGraphics32BlenderNormal
entity: TGraphics32BlenderNormal.GetPixelCombiner
kind: Method
declaration: "procedure GetPixelCombiner(out Func: TPixelCombineEvent); override;"
summary: "Returns nil to signal the caller to use default dmBlend and cmMerge combining."
parameters:
  - name: Func
    type: TPixelCombineEvent
    description: "Assigned nil to signal standard blending."
---

## Description

`GetPixelCombiner` sets `Func` to `nil` for Normal mode, signaling caller components (such as `TBitmap32`) to utilize standard `dmBlend` draw mode and `cmMerge` combine mode.

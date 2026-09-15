---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TCustomGraphics32Blender
entity: TCustomGraphics32Blender.GetPixelCombiner
kind: Method
declaration: "procedure GetPixelCombiner(out Func: TPixelCombineEvent); virtual;"
summary: "Retrieves a TPixelCombineEvent delegate for fast pixel blending callbacks."
parameters:
  - name: Func
    type: TPixelCombineEvent
    description: "Output variable assigned the pixel combiner delegate."
---

## Description

`GetPixelCombiner` retrieves a [[TPixelCombineEvent]] delegate matching the blender's blending algorithm. Subclasses can override this method to supply specialized pixel combination callbacks or set `Func` to `nil` when using optimized core blend modes.

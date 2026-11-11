---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TProgressiveRasterizer
entity: TProgressiveRasterizer.Steps
kind: Property
declaration: "property Steps: Integer read FSteps write SetSteps default 4;"
summary: "Specifies the number of progressive refinement passes and initial power-of-two subsampling block size."
---

## Description

`Steps` controls the number of progressive refinement passes performed by `TProgressiveRasterizer`.

The initial pass fills rectangular blocks of size $2^{\text{Steps}} \times 2^{\text{Steps}}$ pixels (for example, `Steps = 4` yields $16 \times 16$ pixel blocks). Each subsequent pass divides the step size by 2 until single-pixel resolution is reached.

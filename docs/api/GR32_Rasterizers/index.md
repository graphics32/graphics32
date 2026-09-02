---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: GR32_Rasterizers
kind: Unit
summary: "Provides rasterization algorithms for rendering continuous samplers and coordinate mappings into bitmap pixel buffers."
---

## Description

The `GR32_Rasterizers` unit defines the rasterization infrastructure in Graphics32. Rasterizers bridge spatial samplers ([[TCustomSampler]]) and pixel buffers ([[TCustomBitmap32]]), evaluating surface samples across coordinates and writing formatted pixel colors to the target bitmap.

Key components in `GR32_Rasterizers` include:

- **Base Class**: [[TRasterizer]] provides abstract sampling loop mechanics, blend mode handling, and thread-safe persistent property management.
- **Regular Rasterization**: [[TRegularRasterizer]] evaluates samples sequentially scanline by scanline.
- **Fractal / Swizzling Rasterization**: [[TSwizzlingRasterizer]] visits pixel coordinates along space-filling fractal curves to optimize CPU cache locality.
- **Progressive Subsampling**: [[TProgressiveRasterizer]] performs multi-step subsampling for fast interactive coarse previews followed by refinement.
- **Tesseral & Contour Sampling**: [[TTesseralRasterizer]] applies recursive divide-and-conquer block decomposition, while [[TContourRasterizer]] traces pixel contours by color intensity difference.
- **Draft Rendering**: [[TDraftRasterizer]] renders coarse pixel blocks for low-cost live preview.
- **Multi-Threaded Rasterization**: Parallel scanline algorithms using system thread pools or worker threads ([[TMultithreadedRegularRasterizer]], [[TParallelRegularRasterizer]], [[TTaskRegularRasterizer]], [[TThreadRegularRasterizer]]).
- **Combination Infrastructure**: [[TCombineInfo]] and helper routines for configuring drawing mode, master alpha, and pixel combination callbacks during sampling.

[members]

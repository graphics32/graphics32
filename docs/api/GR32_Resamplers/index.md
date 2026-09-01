---
layout: doc
docType: api
unit: GR32_Resamplers
entity: GR32_Resamplers
kind: Unit
summary: "Provides bitmap resampling, spatial interpolation kernels, super-sampling, discrete convolution, morphological filters, and block transfers."
---

## Description

The `GR32_Resamplers` unit implements high-performance image resampling, spatial filtering, neighborhood sampling, and block transfer operations in Graphics32.

It includes:
- **Resamplers**: Fast nearest-neighbor ([[TNearestResampler]]), bilinear ([[TLinearResampler]]), draft downscaling ([[TDraftResampler]]), and high-quality arbitrary kernel resampling ([[TKernelResampler]]).
- **Spatial Reconstruction Kernels**: A comprehensive collection of filter kernels including Box ([[TBoxKernel]]), Triangle/Linear ([[TLinearKernel]]), Cosine ([[TCosineKernel]]), B-Spline ([[TSplineKernel]]), Mitchell-Netravali ([[TMitchellKernel]]), Keys Cubic ([[TCubicKernel]]), Hermite ([[THermiteKernel]]), Sinsh ([[TSinshKernel]]), Gaussian ([[TGaussianKernel]]), and Windowed-Sinc kernels ([[TAlbrechtKernel]], [[TLanczosKernel]], [[TBlackmanKernel]], [[THannKernel]], [[THammingKernel]]).
- **Nested Samplers & Coordinate Transformers**: Sampler chaining via [[TNestedSampler]], coordinate transformation via [[TTransformer]], uniform [[TSuperSampler]], and recursive adaptive [[TAdaptiveSuperSampler]].
- **Neighborhood & Morphological Samplers**: Discrete convolution ([[TConvolver]]), bilateral/selective convolution ([[TSelectiveConvolver]]), morphological dilation ([[TDilater]]), erosion ([[TEroder]]), expansion ([[TExpander]]), and contraction ([[TContracter]]).
- **Block Transfer Functions**: High-performance unscaled ([[BlockTransfer]], [[BlockTransferX]], [[BlendTransfer]]) and scaled ([[StretchTransfer]]) bitmap transfer routines.

[members]

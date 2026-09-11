---
title: "Image Resampling & High-Quality Scaling"
---

# Image Resampling & High-Quality Scaling

Scaling images up or down is a fundamental requirement in image editors, games, and UI frameworks. While standard GDI scaling often produces pixelated ("blocky") upscaling or aliased ("jagged") downscaling, Graphics32 includes a flexible, high-performance resampling architecture supporting advanced mathematical convolution kernels.

In this tutorial, you will learn how to choose resamplers and filter kernels to scale images with smooth anti-aliased quality.

---

## 1. Quick Stretching vs High-Quality Resampling

Graphics32 supports two scaling methods:

1. **`Draw` / `BlockTransfer` (Nearest-Neighbor or Bilinear Interpolation Stretching)**: Extremely fast, ideal for retro pixel-art games or quick preview rendering.
2. **`Resample` / `TKernelResampler` (High-Quality Kernel Scaling)**: Evaluates mathematical windowed sinc/cubic functions across surrounding pixels for smooth, continuous scaling.

```pascal
uses
  GR32, GR32_Resamplers;

var
  Src, Dst: TBitmap32;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  try
    Src.LoadFromFile('photo_input.png');

    // Prepare destination bitmap (e.g. 50% scale down)
    Dst.SetSize(Src.Width div 2, Src.Height div 2, False);

    // --- Approach 1: Fast Nearest-Neighbor Stretch ---
    // Fast, but can show aliasing stair-steps
    Dst.Draw(Dst.BoundsRect, Src.BoundsRect, Src);
    Dst.SaveToFile('output_nearest.png');

  finally
    Dst.Free;
    Src.Free;
  end;
end;
```

---

## 2. Setting Up Resamplers and Kernels

To enable high-quality resampling, attach a resampler class to your source bitmap using `TResamplerClass` helper functions or by creating resampler objects directly.

### Common Resamplers
- **`TNearestResampler`**: Nearest pixel selection (fastest, no interpolation).
- **`TLinearResampler`**: Bilinear interpolation (fast, smooth, slightly blurry when downsampling).
- **`TKernelResampler`**: Convolution filtering using customizable kernel window functions (highest quality).

```pascal
var
  Src, Dst: TBitmap32;
  KernelResampler: TKernelResampler;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  try
    Src.LoadFromFile('photo.jpg');
    Dst.SetSize(800, 600, False);

    // Attach a Kernel Resampler to Src
    KernelResampler := TKernelResampler.Create(Src);

    // Choose a high-quality filter kernel (Lanczos)
    KernelResampler.Kernel := TLanczosKernel.Create;

    // Perform high-quality resample blit into Dst
    Dst.Draw(Dst.BoundsRect, Src.BoundsRect, Src);

    Dst.SaveToFile('scaled_lanczos.png');
  finally
    Dst.Free;
    Src.Free;
  end;
end;
```

---

## 3. Comparing Popular Filter Kernels

Graphics32 provides built-in filter kernels in `GR32_Resamplers.pas`. Each kernel balances sharpness, smoothness, and computation speed:

```pascal
var
  Resampler: TKernelResampler;
begin
  Resampler := TKernelResampler.Create(SrcBitmap);

  // 1. Lanczos Kernel (TLanczosKernel)
  // Sharpest details; best for general photo downsampling and upsampling
  Resampler.Kernel := TLanczosKernel.Create;

  // 2. Mitchell Kernel (TMitchellKernel)
  // Excellent balance between sharpness and ring-free smoothing
  Resampler.Kernel := TMitchellKernel.Create;

  // 3. Bicubic Kernel (TCubicKernel)
  // Smooth cubic interpolation; standard for graphic design tools
  Resampler.Kernel := TCubicKernel.Create;

  // 4. Gaussian Kernel (TGaussianKernel)
  // Smooth, blur-style kernel; great for soft shadows and glow effects
  Resampler.Kernel := TGaussianKernel.Create;

  // 5. Box Kernel (TBoxKernel)
  // Simple box averaging; fast for downsampling
  Resampler.Kernel := TBoxKernel.Create;
end;
```

---

## Summary

In this tutorial, you learned:
1. The distinction between fast nearest-neighbor stretching and high-quality kernel resampling.
2. How to attach a `TKernelResampler` to a source `TBitmap32`.
3. The strengths of popular kernels (`TLanczosKernel`, `TMitchellKernel`, `TCubicKernel`).


Next, continue to [Affine Transformations](./affine-transformations) to learn how to rotate, scale, skew, and translate bitmaps!

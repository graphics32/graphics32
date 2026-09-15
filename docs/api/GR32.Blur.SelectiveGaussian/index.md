---
layout: doc
docType: api
unit: GR32.Blur.SelectiveGaussian
entity: GR32.Blur.SelectiveGaussian
kind: Unit
summary: "Provides edge-preserving selective Gaussian blurring for 32-bit bitmaps."
seealso:
  - "[[GR32.Blur]]"
  - "[[GR32_Gamma]]"
---

## Description

The `GR32.Blur.SelectiveGaussian` unit implements **edge-preserving (bilateral-like) Gaussian blurring** for 32-bit ARGB bitmaps.

While standard Gaussian blurring ([[Blur32]]) averages pixels across a spatial neighborhood regardless of color boundaries, causing sharp edges and fine details to become uniformly blurry, selective Gaussian blurring filters pixels conditionally based on color similarity.

---

## Core Concept: Spatial Radius vs. Color Delta

Selective Gaussian blurring evaluates neighbor pixels within a spatial neighborhood (defined by `Radius`) but includes a neighbor pixel in the weighted Gaussian average **only if its color intensity differs from the center reference pixel by no more than a threshold value (`Delta`)**.

::: center
![](/images/blur-selective-delta.svg)
:::

### 1. Spatial Blur Radius (`Radius`)
The `Radius` parameter defines the spatial neighborhood (in pixels) evaluated around each pixel. Larger radius values allow the filter to consider more surrounding pixels for smoothing, but increase computational complexity.

### 2. Color Intensity Threshold (`Delta`)
The `Delta` parameter (typically $1 \dots 255$) specifies the maximum allowable color channel difference between neighbor pixels and the center reference pixel:

$$|C_{\text{sample}} - C_{\text{ref}}| \le \text{Delta}$$

* **Small Delta (e.g. 5..20)**: Only pixels with nearly identical colors are averaged. Sharp edges, object outlines, and high-contrast details are preserved completely intact while subtle noise, gradients, and flat surface textures are smoothly blurred.
* **Large Delta ($\ge 255$)**: All pixels within the spatial radius satisfy the threshold, causing the selective blur to behave like standard spatial Gaussian blurring.

---

## Practical Applications

* **JPEG Artifact & Noise Reduction**: Removes blocking, ringing, and mosquito noise from compressed photos while maintaining crisp object edges and text boundaries.
* **Skin & Surface Smoothing**: Smooths skin tones and surface textures in photographic retouching without blurring eyes, hair, or sharp image contours.
* **Cartoon / Posterization Preparation**: Flattening color variations within solid regions prior to vectorization or edge detection.

:::: thumbnail
| Before | After |
| --- | --- |
| ![](/images/blur-selective-before.png) | ![](/images/blur-selective-after.png) |
::: caption
Removing JPEG artifacts with Selective Blur. Radius: 5.0, Threshold: 15
:::
::::

---

## Gamma Awareness & Channel Processing

* **Gamma-Aware Blurring**: [[GammaSelectiveGaussianBlur32]] performs color thresholding and kernel weighting in linear light space (using [[GR32_Gamma]]), eliminating dark halo artifacts along high-contrast boundaries.
* **Alpha Channel Policy**: Selective Gaussian blurring processes Red, Green, and Blue channels. By design, the Alpha channel of the target bitmap is preserved without being blurred.

---

## Note on Experimental Implementations

The `GR32.Blur.SelectiveGaussian` unit contains various internal and experimental reference implementations (such as SIMD-optimized passes, cache-accelerated algorithms, and historical GIMP adaptation functions). Only the public entry points ([[SelectiveGaussianBlur32]] and [[GammaSelectiveGaussianBlur32]]) are intended for public use.

[members]

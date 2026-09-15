---
layout: doc
docType: api
unit: GR32.Blur
entity: GR32.Blur
kind: Unit
summary: "Provides high-performance 2D and 1D Gaussian blurring, gamma-aware blur variants, and rectangular/polygonal region masking."
seealso:
  - "[[GR32_Gamma]]"
  - "[[TBitmap32]]"
---

## Description

The `GR32.Blur` unit provides fast Gaussian blurring routines for 32-bit ARGB bitmaps ([[TBitmap32]]). It supports full-image blurs, rectangular sub-region blurs, arbitrary polygonal region blurs, 1D directional horizontal motion blurs, and gamma-aware linear light blurring.

---

## High-Performance Recursive Gaussian Filtering

Standard Gaussian blurring via direct 2D spatial convolution requires $O(R^2)$ kernel multiplications per pixel, where $R$ is the blur radius. For large blur radii, standard convolution becomes computationally prohibitive.

To achieve maximum performance, `GR32.Blur` implements an **Infinite Impulse Response (IIR) recursive Gaussian filter** based on the Young, van Vliet, Triggs, and Sdika algorithms. This recursive filter processes scanlines causally (forward) and anti-causally (backward) using a small set of filter poles:

* **Constant Time Complexity**: Execution speed is $O(1)$ per pixel, independent of the blur radius $R$.
* **Boundary Condition Correction**: Applies exact boundary condition matrices at image edges to eliminate dark border bleeding or edge reflection artifacts.
* **SIMD Acceleration**: Core recursive filter passes utilize SSE2 / SSE4.1 vector operations on supported CPU architectures.

---

## Gaussian Radius vs. Sigma ($\sigma$)

Mathematically, a 1D Gaussian distribution is defined by its standard deviation $\sigma$ (Sigma):

$$G(x) = \frac{1}{\sqrt{2\pi}\sigma} e^{-\frac{x^2}{2\sigma^2}}$$

Because the theoretical Gaussian curve extends infinitely in both directions regardless of $\sigma$, Graphics32 defines the effective **pixel radius** ($R$) as the distance at which kernel weights drop below 1 pixel precision threshold.

`GR32.Blur` provides conversion constants ([[GaussianRadiusToSigma]] and [[GaussianSigmaToRadius]]) to convert between mathematical $\sigma$ and user-facing pixel radius $R$:

$$R = \sigma \cdot \text{GaussianSigmaToRadius}$$
$$\sigma = R \cdot \text{GaussianRadiusToSigma}$$

Where $\text{GaussianRadiusToSigma} \approx 0.30038663$.

---

## Gamma-Aware Blurring

Standard digital image pixels are stored in non-linear gamma or sRGB color space. Performing spatial blurring directly on non-linear color values introduces physical errors:

* **Dark Fringe Artifacts**: High-contrast edges (such as white text on a dark background) develop an unnaturally dark ring or muddy border when blurred in gamma space.
* **Luminance Loss**: Intermediate blended values underestimate physical photon energy.

`GR32.Blur` provides **gamma-aware blur routines** ([[GammaBlur32]] and [[GammaHorizontalBlur32]]). These routines convert color channels into linear light space (using precomputed tables in [[GR32_Gamma]]), perform alpha premultiplication and recursive filtering in linear space, and then convert the result back to gamma/sRGB space.

::: center
| No blur | Blur without gamma | Blur with gamma |
|:--:|:--:|:--:|
| ![](/images/blur-gamma-noblur.png) | ![](/images/blur-gamma-nogamma.png) | ![](/images/blur-gamma-withgamma.png) |

*Blur: 12.0px, Gamma: 1.4*
:::

---

## Region and Rectangular Masking

When blurring a sub-region ([[Blur32]] or [[GammaBlur32]] with `TRect` or `TArrayOfFloatPoint`), `GR32.Blur` dynamically selects an optimal execution path:

1. **Local Sub-Bitmap Copy**: If the target bounding box covers less than 75% of the total bitmap area, the unit extracts only the target region into a temporary buffer, applies an in-place blur, and composits the result back using a [[TBitmapPolygonFiller]].
2. **Full-Bitmap Masked Blur**: If the region covers most of the bitmap, the full bitmap is blurred and masked back onto the source.

::: center
![](/images/blur-region.png)<br>
*Removing mistakes with region blur*
:::

---

## Alpha Channel Handling

All blur routines in `GR32.Blur` process all four 8-bit channels ($R, G, B, A$). Blurring the Alpha channel produces smooth, antialiased soft shadow edges. If alpha blurring is not desired for a specific application, save and restore the alpha channel of the [[TBitmap32]] after calling the blur routine.

---

## Minimum Blur Radius

The global variable [[Blur32MinRadius]] (default $0.5$ pixels) defines the threshold below which blur operations are skipped:
* If $\text{Radius} < \text{Blur32MinRadius}$, out-of-place blur calls perform a fast direct copy, while in-place calls exit immediately.

[members]

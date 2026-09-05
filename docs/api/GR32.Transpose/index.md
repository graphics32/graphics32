---
layout: doc
docType: api
unit: GR32.Transpose
entity: GR32.Transpose
kind: Unit
summary: "Provides high-performance image matrix transposition algorithms for 32-bit pixel buffers."
---

## Description

The `GR32.Transpose` unit implements fast matrix transposition algorithms operating on 32-bit pixel memory structures (`TBitmap32` and raw `PColor32` pixel arrays).

### What is Transposition?

In digital image processing, **transposition** flips a 2D image matrix across its main diagonal, effectively interchanging its rows and columns:

$$\text{Dst}[x, y] = \text{Src}[y, x]$$

If the source image has dimensions $W \times H$ (width $\times$ height), the transposed destination image will have dimensions $H \times W$.

![200x300 Image Matrix Transposition](/images/transpose_animation.gif)

Because pixel scanlines in contiguous memory are stored horizontally in row-major order, naive matrix transposition suffers from severe CPU cache misses due to non-sequential memory access strides along vertical columns. `GR32.Transpose` optimizes this operation using **cache-oblivious recursive spatial subdivision** and **SSE2 SIMD register unpacking**, achieving optimal CPU cache locality across arbitrary image resolutions.

### Common Use Cases

1. **Fast Image Rotation**: Combined with horizontal or vertical pixel line reversal, transposition enables fast $90^\circ$ and $270^\circ$ image rotation without trigonometric matrix calculations.
2. **Separable 2D Image Filtering**: Many 2D spatial image filters (such as Gaussian blur or box blur) are *separable* into 1D horizontal and vertical passes. By transposing the image between passes, vertical filtering operations can be executed horizontally with maximum CPU cache performance and SIMD vectorization efficiency.

[members]

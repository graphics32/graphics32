# Features

The features of Graphics32, while similar to those in the standard TImage, TBitmap, and TCanvas classes, far surpasses the capabilities of its VCL counterparts. Graphics32 has been written to accelerate and optimize drawing on 32-bit device-independent bitmaps (DIBs) independently from the Windows GDI.

Some of its features include:

  * Fast per-pixel access up to 100 times faster compared to standard TCanvas/TBitmap.
  * High-performance bitmap alpha blending, including per-pixel alpha blending.
  * Pixel, line and polygon antialiasing with sub-pixel accuracy, combined with alpha blending.
  * Arbitrary polygon transformations and custom fillings.
  * Bitmap resampling with high quality reconstruction filters such as Lanczos, Cubic, and Mitchell.
  * A unique state-of-the-art rasterization system.
  * Affine transformations of bitmaps: rotations, scaling, etc with sub-pixel accuracy.
  * Arbitrary projective transformations of bitmaps.
  * Arbitrary remapping transformations of bitmaps. For example for Warping or Morphing.
  * Flexible supersampling implementation for maximum sampling quality.
  * Flicker-free image viewer controls with optimized double buffering via advanced MicroTiles based repaint optimizer.
  * Multiple customizable easy-to-use overlay layers.
  * Locking of bitmaps for safe multi-threading.
  * Platform independent code.
  * Switchable surface-managing back-ends.
  * A property editor for RGB and alpha channel loading.
  * A pluggable image format subsystem for import and export of arbitrary image formats.



###### See also:

  * [Device Independant Bitmaps (Microsoft)](https://docs.microsoft.com/en-us/windows/win32/gdi/device-independent-bitmaps)
  * [Repaint Optimization](https://melander.dk/graphics32/repaint-optimization/)
  * [Sampling and Rasterization](https://melander.dk/graphics32/sampling-and-rasterization/)

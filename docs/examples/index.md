# Examples & Tutorials

Welcome to the Graphics32 Examples & Tutorials section! Whether you are completely new to Graphics32 or looking for practical code snippets to perform common graphics tasks, these beginner-friendly guides provide step-by-step explanations and Pascal code examples.

---

## Tutorial Guides

1. **[Basic Drawing & Shapes](./basic-drawing)**
   Learn how to instantiate `TBitmap32`, clear backgrounds, set bitmap dimensions, draw primitive lines and rectangles, and understand clipping method suffixes (`*S`, `*TS`).

2. **[Alpha Blending & Transparency](./alpha-blending)**
   Explore 32-bit ARGB colors (`TColor32`), translucent color constants, `DrawMode` settings (`dmOpaque`, `dmBlend`), combine modes (`cmAdd`, `cmModulate`), and global `MasterAlpha` controls.

3. **[Fast Pixel Access & Direct Manipulation](./pixel-manipulation)**
   Master high-performance pixel operations using bounds-checked `PixelS`, unchecked `Pixel`, row pointers via `ScanLine[Y]`, and whole-image flat array processing with `Bits`.

4. **[Image Resampling & High-Quality Scaling](./resampling-and-scaling)**
   Understand nearest-neighbor stretching vs convolution resampling, configuring `TKernelResampler`, choosing optimal filter kernels (`Lanczos`, `Mitchell`, `Bicubic`), and using procedural `Resample` helpers.

5. **[Affine Transformations](./affine-transformations)**
   Learn how `TAffineTransformation` matrices work to rotate bitmaps around custom center points, scale, skew, and translate surfaces, and calculate exact transformed bounding boxes with `GetTransformedBounds`.

6. **[Vector Polygons & Path Drawing](./vector-polygons)**
   Discover sub-pixel float coordinates (`TFloatPoint`), anti-aliased polygon rendering with VPR (`PolygonFS`), stroking thick paths with customizable line joins and caps, and multi-contour hole filling.

7. **[Color Gradients & Fill Samplers](./color-gradients)**
   Configure gradient color stops with `TColor32Gradient`, fill vector polygons with `TLinearGradientPolygonFiller` and `TRadialGradientPolygonFiller`, and control gradient repetitions with `WrapMode`.

8. **[Working with Interactive Layers in TImage32](./working-with-layers)**
   Build interactive desktop applications using `TImage32`, add positionable `TBitmapLayer` instances, enable interactive mouse dragging and resizing handles with `TRubberbandLayer`, and manage z-order.

9. **[Loading, Saving & Image Formats](./loading-and-saving)**
   Load and save 32-bit images across PNG, JPEG, BMP, and PSD formats from disk and memory streams, preserve alpha channels with `ResetAlpha`, and convert pixels between VCL/FCL `TBitmap` and `TBitmap32`.

10. **[Repaint Optimization & Custom Canvas Drawing](./custom-painting-repaint)**
    Optimize rendering performance with `BeginUpdate`/`EndUpdate` batching, invalidate partial sub-regions using `Changed(TRect)`, draw non-destructive UI overlays with `TImage32.OnPaintStage`, and build smooth animation loops.

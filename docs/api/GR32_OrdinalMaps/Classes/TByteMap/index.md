---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TByteMap
kind: Class
declaration: "TByteMap = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TByteMap
summary: "Two-dimensional 8-bit unsigned byte map with channel conversion, bitmap rendering, rotation, and downsampling capabilities."
---

## Description

`TByteMap` is a 2D array container for 8-bit unsigned byte values (`Byte`), inheriting from [[TCustomMap]].<br>
It is commonly used for grayscale images, alpha channels, stencil masks, heightmaps, and weight maps.

### Features & Capabilities

- **Bitmap Conversion**:
  - `ReadFrom`: Extracts color channels ($R, G, B, A$, uniform RGB, or weighted intensity) from a [[TCustomBitmap32]].
  - `WriteTo`: Converts byte values back into bitmap pixels or applies a 256-color palette ([[TPalette32]]).
- **Drawing & Blending**:
  - `DrawTo`: Blends byte map data onto a destination `TCustomBitmap32` as alpha modulation for a target color ([[TColor32]]).
- **Arithmetic**:
  - `Add`, `Sub`, `Multiply`: Performs saturated per-pixel scalar arithmetic operations on byte buffer elements.
- **Transformation & Resampling**:
  - `FlipHorz`, `FlipVert`, `Rotate90`, `Rotate180`, `Rotate270`: Performs spatial orientation flips and 90-degree step rotations.
  - `Downsample`: Box-filters and downsamples byte map dimensions by integer scaling factors ($2\times, 3\times, 4\times$, up to $32\times$).

[members]

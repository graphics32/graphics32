---
layout: doc
docType: api
unit: GR32
entity: TBitmap32
kind: Class
declaration: "TBitmap32 = class(TCustomBitmap32)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomBitmap32
  - TBitmap32
summary: "Primary 32-bit ARGB bitmap container class in Graphics32."
---

## Constructors & Destructors

- [Create](./Constructors/Create) — Instantiates a new `TBitmap32` object (grouped overloads).
- [Destroy](./Constructors/Destroy) — Disposes of the bitmap object and frees pixel buffer memory.

## Key Methods

- [Clear](./Methods/Clear) — Fills the bitmap pixel buffer with a specified color.
- [Draw](./Methods/Draw) — Draws a source bitmap or rectangle onto this bitmap.

## Key Properties

- [Pixel](./Properties/Pixel) — Provides direct 2D array access to individual 32-bit ARGB pixel values.
- `Width`: Width of the bitmap in pixels.
- `Height`: Height of the bitmap in pixels.
- `DrawMode`: Specifies alpha blending mode (`dmOpaque`, `dmBlend`, `dmTransparent`, `dmCustom`).

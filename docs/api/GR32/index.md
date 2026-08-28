---
layout: doc
docType: api
unit: GR32
entity: GR32
kind: Unit
summary: "Core foundation of the Graphics32 library containing 32-bit ARGB color definitions, base bitmap classes, and pixel utility routines."
---

## Description

The `GR32` unit is the core foundation of the Graphics32 library. It contains the primary data structures, 32-bit ARGB color definitions (`TColor32`), base bitmap classes (`TBitmap32`, `TCustomBitmap32`), and global pixel utility routines.

---

The complete list of types and functions in `GR32` is too big to list here, but here are some of the more important ones:

## Classes

| Class | Description |
|---|---|
| [[TBitmap32]] | Primary 32-bit ARGB bitmap container class. |
| [[TCustomBitmap32]] | Abstract base class for 32-bit bitmaps with backend rendering support. |
| [[TCustomSampler]] | Abstract base class for pixel color sampling algorithms. |
| [[TCustomResampler]] | Abstract base class for bitmap pixel resampling and interpolation. |

---

## Types & Constants

### TColor32 & TColor32Entry
```pascal
type
  TColor32 = type Cardinal;

  TColor32Entry = packed record
    case Integer of
      0: (B, G, R, A: Byte); // or (R, G, B, A: Byte) depending on the platform
      1: (ARGB: TColor32);
      2: (Planes: array[0..3] of Byte);
      3: (Components: array[TColor32Component] of Byte);
  end;
```
A 32-bit unsigned integer holding ARGB color components.

### Common Color Constants
- `clNone32`: No color = `$000000` (fully transparent black)
- `clBlack32`: Opaque Black
- `clWhite32`: Opaque White
- `clRed32`: Opaque Red
- `clGreen32`: Opaque Green
- `clBlue32`: Opaque Blue
- `clTrWhite32`: 50% transparent White
- `clTrRed32`: 50% transparent Red
- `clTrGreen32`: 50% transparent Green
- `clTrBlue32`: 50% transparent Blue
etc...

---

## Global Functions

- `Color32(A, R, G, B: Byte): TColor32`: Constructs a `TColor32` value from individual byte components.
- `Color32(WinColor: TColor): TColor32`: Constructs a `TColor32` value from a VCL `TColor` value.

# Unit GR32

The `GR32` unit is the core foundation of the Graphics32 library. It contains the primary data structures, 32-bit ARGB color definitions (`TColor32`), base bitmap classes (`TBitmap32`, `TCustomBitmap32`), and global pixel utility routines.

---

## Classes

| Class | Description |
|---|---|
| [TBitmap32](./TBitmap32/) | Primary 32-bit ARGB bitmap container class. |
| [TCustomBitmap32](./TCustomBitmap32/) | Abstract base class for 32-bit bitmaps with backend rendering support. |
| [TNotifiablePersistent](./TNotifiablePersistent/) | Base persistent object providing change notification events. |
| [TCustomSampler](./TCustomSampler/) | Abstract base class for pixel color sampling algorithms. |
| [TCustomResampler](./TCustomResampler/) | Abstract base class for bitmap pixel resampling and interpolation. |

---

## Types & Constants

### TColor32
```pascal
type TColor32 = type Cardinal;
```
A 32-bit unsigned integer holding ARGB color components (`$AARRGGBB`).

### Common Color Constants
- `clBlack32`: `$FF000000`
- `clWhite32`: `$FFFFFFFF`
- `clRed32`: `$FFFF0000`
- `clGreen32`: `$FF007F00`
- `clBlue32`: `$FF0000FF`
- `clTrColor32`: `$00000000` (Fully transparent)

---

## Global Functions

- `Color32(A, R, G, B: Byte): TColor32`: Constructs a `TColor32` value from individual byte components.
- `AlphaComponent(Color: TColor32): Byte`: Extracts the alpha channel component (0..255).
- `RedComponent(Color: TColor32): Byte`: Extracts the red channel component (0..255).
- `GreenComponent(Color: TColor32): Byte`: Extracts the green channel component (0..255).
- `BlueComponent(Color: TColor32): Byte`: Extracts the blue channel component (0..255).

---
layout: doc
docType: api
unit: GR32
entity: TColor32
kind: Type
summary: "32-bit unsigned integer type representing an ARGB color value."
declaration: "type TColor32 = type Cardinal;"
---

## Description

`TColor32` is the primary pixel and color representation in Graphics32. It is a 32-bit unsigned cardinal integer formatted as `$AARRGGBB` in memory (or `$AABBGGRR` when `RGBA_FORMAT` is defined).

## Related Pointer & Array Types

| Type | Declaration | Description |
| --- | --- | --- |
| `PColor32` | `^TColor32` | Pointer to a single `TColor32` color value. |
| `PColor32Array` | `^TColor32Array` | Pointer to an un-sized array of `TColor32` values. |
| `TColor32Array` | `array [0..0] of TColor32` | Un-sized static array type used for scanline indexing. |
| `TArrayOfColor32` | `array of TColor32` | Dynamic array of `TColor32` values. |

## See also

- [[TColor32Entry]]
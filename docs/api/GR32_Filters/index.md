---
layout: doc
docType: api
unit: GR32_Filters
entity: GR32_Filters
kind: Unit
summary: "Provides high-performance pixel filtering, color channel manipulation, bitmask operations, grayscale conversion, and Look-Up Table (LUT) transformations."
---

# Unit GR32_Filters

The `GR32_Filters` unit contains basic image processing filter routines and color manipulation algorithms.

## Types

| Type | Description |
| --- | --- |
| [TLUT8](Types/TLUT8.md) | 256-element Byte array used for 8-bit Look-Up Table (LUT) color transformations. |
| [TLogicalOperator](Types/TLogicalOperator.md) | Logical bitwise operator enumeration (`loXOR`, `loAND`, `loOR`). |

## Routines

| Routine | Description |
| --- | --- |
| [AlphaToGrayscale](Routines/AlphaToGrayscale.md) | Converts a bitmap to grayscale by mapping alpha channel values to RGB channels. |
| [ApplyBitmask](Routines/ApplyBitmask.md) | Performs bitwise logical operations (`loAND`, `loOR`, `loXOR`) between bitmap pixels and a bitmask. |
| [ApplyLUT](Routines/ApplyLUT.md) | Transforms bitmap color channels using an 8-bit Look-Up Table (`TLUT8`). |
| [CheckParams](Routines/CheckParams.md) | Validates bitmap parameters and resizes destination bitmaps to match source dimensions. |
| [ChromaKey](Routines/ChromaKey.md) | Sets alpha component to transparent (`0`) for pixels matching a specific RGB key color. |
| [ColorToGrayscale](Routines/ColorToGrayscale.md) | Converts a color bitmap to grayscale based on pixel luminance (intensity). |
| [CopyComponents](Routines/CopyComponents.md) | Copies specified ARGB color channels from a source bitmap to a destination bitmap. |
| [CreateBitmask](Routines/CreateBitmask.md) | Generates a `TColor32` bitmask corresponding to selected `TColor32Components`. |
| [IntensityToAlpha](Routines/IntensityToAlpha.md) | Maps weighted pixel luminance (intensity) of source pixels to the alpha channel of destination pixels. |
| [Invert](Routines/Invert.md) | Inverts (negates) specified color channels of a bitmap. |
| [InvertRGB](Routines/InvertRGB.md) | Inverts red, green, and blue color channels while preserving the alpha channel. |

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
| [[TLUT8]] | 256-element Byte array used for 8-bit Look-Up Table (LUT) color transformations. |
| [[TLogicalOperator]] | Logical bitwise operator enumeration (`loXOR`, `loAND`, `loOR`). |

## Routines

| Routine | Description |
| --- | --- |
| [[AlphaToGrayscale]] | Converts a bitmap to grayscale by mapping alpha channel values to RGB channels. |
| [[ApplyBitmask]] | Performs bitwise logical operations (`loAND`, `loOR`, `loXOR`) between bitmap pixels and a bitmask. |
| [[ApplyLUT]] | Transforms bitmap color channels using an 8-bit Look-Up Table (`TLUT8`). |
| [[CheckParams]] | Validates bitmap parameters and resizes destination bitmaps to match source dimensions. |
| [[ChromaKey]] | Sets alpha component to transparent (`0`) for pixels matching a specific RGB key color. |
| [[ColorToGrayscale]] | Converts a color bitmap to grayscale based on pixel luminance (intensity). |
| [[CopyComponents]] | Copies specified ARGB color channels from a source bitmap to a destination bitmap. |
| [[CreateBitmask]] | Generates a `TColor32` bitmask corresponding to selected `TColor32Components`. |
| [[IntensityToAlpha]] | Maps weighted pixel luminance (intensity) of source pixels to the alpha channel of destination pixels. |
| [[Invert]] | Inverts (negates) specified color channels of a bitmap. |
| [[InvertRGB]] | Inverts red, green, and blue color channels while preserving the alpha channel. |

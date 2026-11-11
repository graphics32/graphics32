---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: GR32_OrdinalMaps
kind: Unit
summary: "Provides 2D ordinal map structures for managing boolean, byte, word, integer, cardinal, float, and generic data buffers."
---

## Description

The `GR32_OrdinalMaps` unit provides specialized two-dimensional array containers derived from [[TCustomMap]] for storing scalar and ordinal primitive data types.

These classes manage contiguous, dynamically-sized 2D pixel-aligned buffers suitable for storing grayscale masks, heightmaps, alpha channels, weights, lookup tables, and custom per-pixel attributes.

### Core Map Classes

- **`TBooleanMap`**: Bit-packed 2D boolean array (1 bit per pixel).
- **`TByteMap`**: 8-bit unsigned byte map with built-in channel conversions (`ReadFrom`/`WriteTo`), palette mapping, drawing/blending operations, rotation, and integer downsampling.
- **`TWordMap`**: 16-bit unsigned integer map.
- **`TIntegerMap`**: 32-bit signed integer map.
- **`TCardinalMap`**: 32-bit unsigned cardinal integer map.
- **`TFloatMap`**: Floating-point (`TFloat`) map for high-precision analytical calculations and weight fields.
- **`TGenericMap<T>`**: Generic 2D data map for storing arbitrary value types.

[members]

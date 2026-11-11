---
layout: doc
docType: api
unit: GR32_Blend
entity: Premultiply
aliases: [Premultiply32, Unpremultiply32, PremultiplyMem, UnpremultiplyMem]
kind: Procedure
declaration: |
  procedure Premultiply32(Bitmap: TCustomBitmap32);
  procedure Unpremultiply32(Bitmap: TCustomBitmap32);
  procedure PremultiplyMem(Pixels: PColor32; Count: Integer);
  procedure UnpremultiplyMem(Pixels: PColor32; Count: Integer);
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "The bitmap to operate on."
  - name: Pixels
    type: PColor32
    description: "Pointer to the first 32-bit ARGB pixel in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous pixels in the buffer to process."
summary: "Converts pixel color buffers between straight ARGB and premultiplied ARGB formats."
seealso:
  - "[[TCustomBitmap32]]"
---

## Description

These routines perform conversion between straight ARGB (non-premultiplied) and premultiplied ARGB color formats.

### Alpha Premultiplication
In premultiplied ARGB format, each color component (R, G, B) is pre-multiplied by its alpha value:
$$C' = \frac{C \cdot A + 127}{255}$$
Premultiplied colors simplify compositing, linear interpolation, and image filtering.

### Alpha Unpremultiplication
Converts premultiplied ARGB colors back to straight ARGB:
$$C = \frac{C' \cdot 255 + \lfloor A/2 \rfloor}{A}$$
If $A = 0$, the resulting RGB components are set to 0.

### Routines Overview

| Name | Description |
| --- | --- |
| `Premultiply32` | Premultiplies all pixels in a `TCustomBitmap32` instance and notifies listeners via `Bitmap.Changed`. |
| `Unpremultiply32 | Unpremultiplies all pixels in a `TCustomBitmap32` instance and notifies listeners via `Bitmap.Changed`. |
| `PremultiplyMem | Premultiplies `Count` pixels starting at memory address `Pixels`. |
| `UnpremultiplyMem | Unpremultiplies `Count` pixels starting at memory address `Pixels`. |

::: info
`PremultiplyMem` and `UnpremultiplyMem` are actually delegates; At startup, Graphics32 [binds them to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Pixel
kind: Property
scope: Public
aliases: [PixelS, PixelW, PixelX, PixelXS, PixelXW, PixelF, PixelFS, PixelFW, PixelFR, PixelXR]
declaration: |
  property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
  property PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;
  property PixelW[X, Y: Integer]: TColor32 read GetPixelW write SetPixelW;
  property PixelX[X, Y: TFixed]: TColor32 read GetPixelX write SetPixelX;
  property PixelXS[X, Y: TFixed]: TColor32 read GetPixelXS write SetPixelXS;
  property PixelXW[X, Y: TFixed]: TColor32 read GetPixelXW write SetPixelXW;
  property PixelF[X, Y: Single]: TColor32 read GetPixelF write SetPixelF;
  property PixelFS[X, Y: Single]: TColor32 read GetPixelFS write SetPixelFS;
  property PixelFW[X, Y: Single]: TColor32 read GetPixelFW write SetPixelFW;
  property PixelFR[X, Y: Single]: TColor32 read GetPixelFR;
  property PixelXR[X, Y: TFixed]: TColor32 read GetPixelXR;
parameters:
  - name: "X, Y"
    type: "-"
    description: "Pixel coordinates"
summary: "Indexed pixel access properties for reading and writing 32-bit ARGB pixel values across integer, fixed-point, and floating-point coordinates."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
---

## Description

The `Pixel` properties provide indexed access to individual 32-bit ARGB pixel values within the bitmap. Depending on the variant used, pixel access can perform direct unclipped memory access, boundary clipping (safe access), or coordinate wrapping (tiling), accepting integer, 16.16 fixed-point (`TFixed`), or single-precision floating-point (`Single`) coordinates.

Floating-point (`F`) and fixed-point (`X`) coordinate variants operate either on a sub-pixel level using bilinear interpolation or calculate resampled pixel values (`FR`, `XR`) through the active [[Resampler]].

:::: thumbnail
![](/images/pixel-antialias.png "Antialiased pixel")
::: caption
Antialiased pixel
:::
::::

## Variants

| Variant | Coordinate Type | Access Mode | Description |
| --- | --- | --- | --- |
| `Pixel` | `Integer` | Direct / Default | Fast, direct pixel access without boundary checking. Default indexed property (`Bitmap[X, Y]`). |
| `PixelS` | `Integer` | Safe / Clipped | Boundary-clipped pixel access. Reads outside [[ClipRect]] return [[OuterColor]]. Writes outside `ClipRect` are ignored. |
| `PixelW` | `Integer` | Wrapped / Tiled | [[WrapMode\|Coordinate-wrapped]] pixel access modulo bitmap dimensions (`Width` and `Height`). |
| `PixelX` | `TFixed` | Sub-pixel | Sub-pixel access using 16.16 fixed-point coordinates with bilinear interpolation. Direct / unclipped. |
| `PixelXS` | `TFixed` | Sub-pixel Safe | [[ClipRect\|Boundary-clipped]] sub-pixel access using 16.16 fixed-point coordinates with bilinear interpolation. |
| `PixelXW` | `TFixed` | Sub-pixel Wrapped | [[WrapMode\|Coordinate-wrapped]] sub-pixel access using 16.16 fixed-point coordinates with bilinear interpolation. |
| `PixelXR` | `TFixed` | Resampled | Read-only resampled pixel access at 16.16 fixed-point coordinates evaluated using the active [[Resampler]]. |
| `PixelF` | `Single` | Sub-pixel | Sub-pixel access using floating-point coordinates with bilinear interpolation. Direct / unclipped. |
| `PixelFS` | `Single` | Sub-pixel Safe | [[ClipRect\|Boundary-clipped]] sub-pixel access using floating-point coordinates with bilinear interpolation. |
| `PixelFW` | `Single` | Sub-pixel Wrapped | [[WrapMode\|Coordinate-wrapped]] sub-pixel access using floating-point coordinates with bilinear interpolation. |
| `PixelFR` | `Single` | Resampled | Read-only resampled pixel access at floating-point coordinates evaluated using the active [[Resampler]]. |

## Example

```pascal
// Integer coordinates (direct and safe)
Color := Bitmap[10, 20];
Bitmap.PixelS[-1, 5] := clRed32;

// Sub-pixel floating-point coordinates
Color := Bitmap.PixelF[10.5, 20.25];
```
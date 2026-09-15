---
layout: doc
docType: api
unit: GR32.Blur
entity: HorizontalBlur32
aliases: [GammaHorizontalBlur32]
kind: Variable
declaration: |
  procedure HorizontalBlur32(ASource, ADest: TBitmap32; Radius: TFloat);
  procedure GammaHorizontalBlur32(ASource, ADest: TBitmap32; Radius: TFloat);
parameters:
  - name: ASource
    type: TBitmap32
    description: "Source 32-bit bitmap to read pixels from."
  - name: ADest
    type: TBitmap32
    description: "Destination 32-bit bitmap to receive the horizontally blurred output."
  - name: Radius
    type: TFloat
    description: "Blur radius in pixels."
summary: "Directional 1D horizontal Gaussian blur."
seealso:
  - "[[Blur32]]"
  - "[[GammaBlur32]]"
  - "[[GaussianRadiusToSigma]]"
---

## Description

`HorizontalBlur32` and `GammaHorizontalBlur32` performs a 1D horizontal-only Gaussian blur across bitmap scanlines.

These routines apply the recursive Gaussian filter along horizontal rows without performing the vertical filtering pass, producing horizontal streak or motion blur effects.

### Variant Summary

| Function | Description |
| --- | --- |
| `HorizontalBlur32` | 1D horizontal Gaussian blur in standard non-linear sRGB space. |
| `GammaHorizontalBlur32` | 1D horizontal Gaussian blur in linear light space (gamma-aware). |

## Example

```pascal
var
  Src, Dst: TBitmap32;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  try
    Src.LoadFromFile('photo.png');

    // Apply horizontal motion blur with a 15.0 pixel radius
    HorizontalBlur32(Src, Dst, 15.0);

    Dst.SaveToFile('horizontal_motion_blur.png');
  finally
    Src.Free;
    Dst.Free;
  end;
end;
```

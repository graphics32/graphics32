---
layout: doc
docType: api
unit: GR32.Blur.SelectiveGaussian
entity: SelectiveGaussianBlur32
kind: Procedure
declaration: "procedure SelectiveGaussianBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Delta: Integer);"
parameters:
  - name: ASource
    type: TBitmap32
    description: "Source 32-bit bitmap to read pixels from."
  - name: ADest
    type: TBitmap32
    description: "Destination 32-bit bitmap to receive the edge-preserved blurred output."
  - name: Radius
    type: TFloat
    description: "Spatial blur radius in pixels."
  - name: Delta
    type: Integer
    description: "Maximum color channel difference threshold (`0..255`)."
summary: "Performs edge-preserving selective Gaussian blurring on a 32-bit bitmap."
seealso:
  - "[[GammaSelectiveGaussianBlur32]]"
  - "[[Blur32]]"
---

## Description

`SelectiveGaussianBlur32` performs edge-preserving Gaussian blurring on 32-bit ARGB bitmaps.

Neighbor pixels within the spatial window (`Radius`) are accumulated into the weighted Gaussian sum only if their color channel difference from the center pixel is within the specified `Delta` threshold:

$$|C_{\text{sample}} - C_{\text{ref}}| \le \text{Delta}$$

### Behavior Summary

* **Preserving Details**: Setting a modest `Delta` (e.g. $10 \dots 30$) smooths noise, JPEG compression artifacts, and flat region variations while leaving sharp contrast edges completely intact.
* **Alpha Channel**: The Alpha channel is preserved without being blurred.

## Example

```pascal
var
  Src, Dst: TBitmap32;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  try
    Src.LoadFromFile('photo_noisy.png');

    // Smooth surface noise (Radius=4.0) while preserving sharp edges (Delta=15)
    SelectiveGaussianBlur32(Src, Dst, 4.0, 15);

    Dst.SaveToFile('photo_denoised.png');
  finally
    Src.Free;
    Dst.Free;
  end;
end;
```

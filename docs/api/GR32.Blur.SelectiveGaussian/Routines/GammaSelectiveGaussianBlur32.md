---
layout: doc
docType: api
unit: GR32.Blur.SelectiveGaussian
entity: GammaSelectiveGaussianBlur32
kind: Procedure
declaration: "procedure GammaSelectiveGaussianBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Delta: Integer);"
parameters:
  - name: ASource
    type: TBitmap32
    description: "Source 32-bit bitmap to read pixels from."
  - name: ADest
    type: TBitmap32
    description: "Destination 32-bit bitmap to receive the linear-light blurred output."
  - name: Radius
    type: TFloat
    description: "Spatial blur radius in pixels."
  - name: Delta
    type: Integer
    description: "Maximum color channel difference threshold (`0..255`)."
summary: "Performs gamma-aware, edge-preserving selective Gaussian blurring in linear light space."
seealso:
  - "[[SelectiveGaussianBlur32]]"
  - "[[GammaBlur32]]"
  - "[[GR32_Gamma]]"
---

## Description

`GammaSelectiveGaussianBlur32` performs edge-preserving selective Gaussian blurring in **linear light space**.

Pixel channels are converted into linear light space before color thresholding and kernel weighting, preventing dark fringe artifacts along high-contrast boundaries.

### Behavior Summary

* **Linear Light Precision**: Performs gamma decoding and encoding via precomputed tables constructed from the current Gamma settings ([[GAMMA_VALUE]], [[GAMMA_IS_SRGB]]).
* **Detail Preservation**: Neighbor pixels within `Radius` are included in the Gaussian average only if their color intensity difference from the reference pixel does not exceed `Delta`.
* **Alpha Channel**: The Alpha channel is preserved without being blurred.
 
## Example

```pascal
var
  Src, Dst: TBitmap32;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  try
    Src.LoadFromFile('compressed_art.png');

    // Gamma-correct selective blur to smooth artifacts without dark border halos
    GammaSelectiveGaussianBlur32(Src, Dst, 5.0, 20);

    Dst.SaveToFile('compressed_art_clean.png');
  finally
    Src.Free;
    Dst.Free;
  end;
end;
```

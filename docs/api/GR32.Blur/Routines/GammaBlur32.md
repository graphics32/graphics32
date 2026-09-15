---
layout: doc
docType: api
unit: GR32.Blur
entity: GammaBlur32
kind: Routine
summary: "Blurs a 32-bit bitmap in linear light space to prevent dark fringe artifacts on high-contrast edges."
overloads:
  - signature: "procedure GammaBlur32(ASource, ADest: TBitmap32; Radius: TFloat); overload;"
    summary: "Blurs the source bitmap in linear light space and writes the result into the destination bitmap."
    parameters:
      - name: ASource
        type: TBitmap32
        description: "Source 32-bit bitmap to read pixels from."
      - name: ADest
        type: TBitmap32
        description: "Destination 32-bit bitmap to receive the blurred output."
      - name: Radius
        type: TFloat
        description: "Blur radius in pixels."

  - signature: "procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat); overload;"
    summary: "Blurs the entire bitmap in-place using linear light space."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Bitmap to blur in-place."
      - name: Radius
        type: TFloat
        description: "Blur radius in pixels."

  - signature: "procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect); overload;"
    summary: "Blurs a rectangular sub-region of a bitmap in-place using linear light space."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Bitmap containing the sub-region to blur."
      - name: Radius
        type: TFloat
        description: "Blur radius in pixels."
      - name: Bounds
        type: TRect
        description: "Bounding rectangle defining the target sub-region."

  - signature: "procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint); overload;"
    summary: "Blurs an arbitrary polygonal region of a bitmap in-place using linear light space."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Bitmap containing the polygonal region to blur."
      - name: Radius
        type: TFloat
        description: "Blur radius in pixels."
      - name: Region
        type: TArrayOfFloatPoint
        description: "Array of 2D floating-point points defining the polygon vertices."
seealso:
  - "[[Blur32]]"
  - "[[HorizontalBlur32]]"
  - "[[GR32_Gamma]]"
  - "[[GaussianRadiusToSigma]]"
---

## Description

`GammaBlur32` performs gamma-aware Gaussian blurring on 32-bit ARGB bitmaps.

Unlike [[Blur32]], which operates directly on non-linear sRGB pixel values, `GammaBlur32` converts pixel channels into linear light space before applying the recursive filter passes, and converts the result back to sRGB space afterwards.

### Why Use Gamma-Aware Blurring?

Standard blurring on non-linear sRGB pixels underestimates physical photon luminance along transitions between bright highlights and dark shadows. This results in noticeable **dark fringe artifacts** (a dark halo along white-on-black text or antialiased shapes).

By performing channel filtering in linear light space, `GammaBlur32` ensures physically accurate light blending and clean, natural edge falloffs.

## Example

```pascal
var
  Bmp: TBitmap32;
begin
  Bmp := TBitmap32.Create;
  try
    Bmp.LoadFromFile('high_contrast_text.png');

    // Perform gamma-correct Gaussian blur to avoid dark edge halos
    GammaBlur32(Bmp, 8.0);

    Bmp.SaveToFile('gamma_blurred.png');
  finally
    Bmp.Free;
  end;
end;
```

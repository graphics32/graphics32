---
layout: doc
docType: api
unit: GR32.Blur
entity: Blur32
kind: Routine
summary: "Blurs a 32-bit bitmap using a fast recursive Gaussian filter."
overloads:
  - signature: "procedure Blur32(ASource, ADest: TBitmap32; Radius: TFloat); overload;"
    summary: "Blurs the source bitmap and writes the result into the destination bitmap."
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

  - signature: "procedure Blur32(Bitmap: TBitmap32; Radius: TFloat); overload;"
    summary: "Blurs the entire bitmap in-place."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Bitmap to blur in-place."
      - name: Radius
        type: TFloat
        description: "Blur radius in pixels."

  - signature: "procedure Blur32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect); overload;"
    summary: "Blurs a rectangular sub-region of a bitmap in-place."
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

  - signature: "procedure Blur32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint); overload;"
    summary: "Blurs an arbitrary polygonal region of a bitmap in-place."
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
  - "[[GammaBlur32]]"
  - "[[HorizontalBlur32]]"
  - "[[GaussianRadiusToSigma]]"
  - "[[Blur32MinRadius]]"
---

## Description

`Blur32` performs spatial Gaussian blurring on 32-bit ARGB bitmaps using a high-performance Infinite Impulse Response (IIR) recursive Gaussian filter.

The filter execution time is $O(1)$ per pixel, making performance independent of the specified `Radius`.

### Radius Threshold
If `Radius` is less than [[Blur32MinRadius]] (default $0.5$ pixels), `Blur32` skips filter calculations. For out-of-place calls (`ASource`, `ADest`), `ASource` is copied directly to `ADest`. For in-place calls, the procedure exits immediately.

### Channel Processing
All four color channels (Red, Green, Blue, Alpha) are processed. Blurring the Alpha channel produces smooth antialiased region borders and drop shadows.

## Example

```pascal
var
  Bmp: TBitmap32;
begin
  Bmp := TBitmap32.Create;
  try
    Bmp.LoadFromFile('input.png');

    // Blur entire bitmap in-place with a 5.0 pixel Gaussian radius
    Blur32(Bmp, 5.0);

    Bmp.SaveToFile('blurred.png');
  finally
    Bmp.Free;
  end;
end;
```

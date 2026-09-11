---
layout: doc
docType: api
unit: GR32_Gamma
entity: ApplyCustomGamma
kind: Function
summary: "Applies a custom lookup table or custom power-law gamma value to a color, pixel array, or bitmap."
overloads:
  - signature: "function ApplyCustomGamma(Color: TColor32; GammaTable: TGammaTable8Bit): TColor32; overload;"
    summary: "Applies a custom 256-byte lookup table to the Red, Green, and Blue channels of a single TColor32 value."
    parameters:
      - name: Color
        type: TColor32
        description: "Input 32-bit ARGB color value."
      - name: GammaTable
        type: TGammaTable8Bit
        description: "Custom 256-byte gamma lookup table array."
    returns:
      - type: TColor32
        description: "Transformed color value with original alpha channel preserved."

  - signature: "procedure ApplyCustomGamma(Color: PColor32Array; Length: Integer; GammaTable: TGammaTable8Bit); overload;"
    summary: "Applies a custom 256-byte lookup table in-place to a contiguous buffer array of TColor32 pixels."
    parameters:
      - name: Color
        type: PColor32Array
        description: "Pointer to contiguous array of TColor32 pixels."
      - name: Length
        type: Integer
        description: "Number of pixels to process in the array."
      - name: GammaTable
        type: TGammaTable8Bit
        description: "Custom 256-byte gamma lookup table array."

  - signature: "procedure ApplyCustomGamma(Bitmap: TBitmap32; GammaTable: TGammaTable8Bit); overload;"
    summary: "Applies a custom 256-byte lookup table in-place to all pixels in a TBitmap32."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Target bitmap to transform in-place."
      - name: GammaTable
        type: TGammaTable8Bit
        description: "Custom 256-byte gamma lookup table array."

  - signature: "procedure ApplyCustomGamma(Bitmap: TBitmap32; Gamma: Double); overload;"
    summary: "Applies a specified power-law gamma exponent in-place to all pixels in a TBitmap32."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Target bitmap to transform in-place."
      - name: Gamma
        type: Double
        description: "Power-law gamma exponent to apply."
seealso:
  - "[[ApplyGamma]]"
  - "[[ApplyInvGamma]]"
  - "[[TGammaTable8Bit]]"
---

## Description

`ApplyCustomGamma` transforms color channels using a custom [[TGammaTable8Bit]] lookup table or floating-point power-law exponent `Gamma`.

The 2-parameter bitmap overload `ApplyCustomGamma(Bitmap, Gamma)` checks whether `GAMMA_VALUE` matches `Gamma`. If `GAMMA_VALUE = Gamma`, it reuses [[GAMMA_ENCODING_TABLE]] directly for optimal performance; otherwise, it generates a temporary table via [[SetGamma]] and applies it to the bitmap.

## Example

```pascal
var
  CustomTable: TGammaTable8Bit;
begin
  SetGamma(2.2, CustomTable);
  ApplyCustomGamma(MyBitmap, CustomTable);

  // Or directly pass a gamma exponent
  ApplyCustomGamma(MyBitmap, 1.8);
end;
```

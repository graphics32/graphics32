---
layout: doc
docType: api
unit: GR32_Gamma
entity: ApplyGamma
kind: Function
summary: "Applies gamma encoding (using global GAMMA_ENCODING_TABLE) to a color, pixel array, or bitmap."
overloads:
  - signature: "function ApplyGamma(Color: TColor32): TColor32; overload;"
    summary: "Applies global GAMMA_ENCODING_TABLE to the Red, Green, and Blue channels of a single TColor32 value."
    parameters:
      - name: Color
        type: TColor32
        description: "Input 32-bit ARGB color value."
    returns:
      - type: TColor32
        description: "Gamma-encoded color value with original alpha channel preserved."

  - signature: "procedure ApplyGamma(Color: PColor32Array; Length: Integer); overload;"
    summary: "Applies global GAMMA_ENCODING_TABLE in-place to a contiguous buffer array of TColor32 pixels."
    parameters:
      - name: Color
        type: PColor32Array
        description: "Pointer to contiguous array of TColor32 pixels."
      - name: Length
        type: Integer
        description: "Number of pixels to process in the array."

  - signature: "procedure ApplyGamma(Bitmap: TBitmap32); overload;"
    summary: "Applies global GAMMA_ENCODING_TABLE in-place to all pixels in a TBitmap32."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Target bitmap to transform in-place."
seealso:
  - "[[GAMMA_ENCODING_TABLE]]"
  - "[[ApplyInvGamma]]"
  - "[[ApplyCustomGamma]]"
---

## Description

`ApplyGamma` applies gamma encoding to RGB channels using the current global [[GAMMA_ENCODING_TABLE]]. Alpha channel values remain unchanged.

## Example

```pascal
var
  EncodedColor: TColor32;
begin
  EncodedColor := ApplyGamma(clRed32);
  ApplyGamma(MyBitmap);
end;
```

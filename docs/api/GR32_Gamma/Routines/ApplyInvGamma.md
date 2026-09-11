---
layout: doc
docType: api
unit: GR32_Gamma
entity: ApplyInvGamma
kind: Function
summary: "Applies gamma decoding (using global GAMMA_DECODING_TABLE) to a color, pixel array, or bitmap."
overloads:
  - signature: "function ApplyInvGamma(Color: TColor32): TColor32; overload;"
    summary: "Applies global GAMMA_DECODING_TABLE to the Red, Green, and Blue channels of a single TColor32 value."
    parameters:
      - name: Color
        type: TColor32
        description: "Input 32-bit ARGB color value."
    returns:
      - type: TColor32
        description: "Gamma-decoded color value in linear light space with original alpha channel preserved."

  - signature: "procedure ApplyInvGamma(Color: PColor32Array; Length: Integer); overload;"
    summary: "Applies global GAMMA_DECODING_TABLE in-place to a contiguous buffer array of TColor32 pixels."
    parameters:
      - name: Color
        type: PColor32Array
        description: "Pointer to contiguous array of TColor32 pixels."
      - name: Length
        type: Integer
        description: "Number of pixels to process in the array."

  - signature: "procedure ApplyInvGamma(Bitmap: TBitmap32); overload;"
    summary: "Applies global GAMMA_DECODING_TABLE in-place to all pixels in a TBitmap32."
    parameters:
      - name: Bitmap
        type: TBitmap32
        description: "Target bitmap to transform in-place."
seealso:
  - "[[GAMMA_DECODING_TABLE]]"
  - "[[ApplyGamma]]"
  - "[[ApplyCustomGamma]]"
---

## Description

`ApplyInvGamma` applies gamma decoding to RGB channels using the current global [[GAMMA_DECODING_TABLE]], converting pixels into linear light space. Alpha channel values remain unchanged.

## Example

```pascal
var
  LinearColor: TColor32;
begin
  LinearColor := ApplyInvGamma(MyColor);
  ApplyInvGamma(MyBitmap);
end;
```

---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.ResetAlpha
kind: Method
scope: Public
summary: "Resets alpha values of all pixels to $FF (fully opaque) or a specified alpha value."
overloads:
  - signature: "procedure ResetAlpha; overload;"
    summary: "Resets the alpha component of all pixels in the bitmap to $FF (fully opaque)."
  - signature: "procedure ResetAlpha(const AlphaValue: Byte); overload;"
    summary: "Resets the alpha component of all pixels in the bitmap to AlphaValue."
    parameters:
      - name: AlphaValue
        type: Byte
        description: "8-bit alpha value (0 to 255)."
---

## Description

`ResetAlpha` sets the alpha channel of all pixels without altering RGB color components.

## Example

```pascal
Bitmap.ResetAlpha(255);
```

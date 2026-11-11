---
layout: doc
docType: api
unit: GR32_Gamma
entity: SetInv_sRGB
kind: Procedure
declaration: "procedure SetInv_sRGB(var GammaTable: TGammaTable8Bit);"
summary: "Populates a custom 256-byte lookup table with inverse sRGB transfer function values (sRGB to linear light)."
parameters:
  - name: GammaTable
    type: TGammaTable8Bit
    description: "Target 256-byte table array to receive inverse sRGB values."
seealso:
  - "[[Set_sRGB]]"
  - "[[GAMMA_DECODING_TABLE]]"
---

## Description

`SetInv_sRGB` populates a custom [[TGammaTable8Bit]] array with inverse sRGB (sRGB space to linear light) transfer function values.

## Formula

For a normalized channel input $V = i / 255$:

$$\text{GammaTable}[i] = \begin{cases} \text{Round}\left(255 \cdot \frac{V}{12.92}\right) & \text{if } V < 0.004045 \\ \text{Round}\left(255 \cdot \left(\frac{V + 0.055}{1.055}\right)^{2.4}\right) & \text{if } V \ge 0.004045 \end{cases}$$

## Example

```pascal
var
  InvSRGBTable: TGammaTable8Bit;
begin
  SetInv_sRGB(InvSRGBTable);
  ApplyCustomGamma(Bitmap, InvSRGBTable);
end;
```

---
layout: doc
docType: api
unit: GR32_Gamma
entity: Set_sRGB
kind: Procedure
summary: "Initializes global or custom lookup tables with standard sRGB IEC 61966-2-1 transfer functions."
overloads:
  - signature: "procedure Set_sRGB; overload;"
    summary: "Populates global encoding and decoding tables with sRGB transfer functions, sets GAMMA_IS_SRGB to True, and notifies change listeners."
  - signature: "procedure Set_sRGB(var GammaTable: TGammaTable8Bit); overload;"
    summary: "Populates a custom 256-byte lookup table with standard forward sRGB transfer function values (linear light to sRGB space)."
    parameters:
      - name: GammaTable
        type: TGammaTable8Bit
        description: "Target 256-byte table array to receive sRGB values."
seealso:
  - "[[SetInv_sRGB]]"
  - "[[SetGamma]]"
  - "[[GAMMA_IS_SRGB]]"
---

## Description

`Set_sRGB` populates gamma lookup tables according to the standard sRGB (IEC 61966-2-1) piecewise transfer function.

The parameterless overload populates [[GAMMA_ENCODING_TABLE]] with the forward sRGB curve (linear $\to$ sRGB) and [[GAMMA_DECODING_TABLE]] with the inverse sRGB curve (sRGB $\to$ linear), sets [[GAMMA_IS_SRGB]] to `True`, and invokes delegates registered with [[RegisterGammaChangeNotification]].

## Formula

For a normalized channel input $V = i / 255$:

$$\text{GammaTable}[i] = \begin{cases} \text{Round}\left(255 \cdot 12.92 \cdot V\right) & \text{if } V < 0.0031308 \\ \text{Round}\left(255 \cdot \left(1.055 \cdot V^{1/2.4} - 0.055\right)\right) & \text{if } V \ge 0.0031308 \end{cases}$$

## Example

```pascal
// Configure global gamma tables to use standard sRGB transfer functions
Set_sRGB;
```

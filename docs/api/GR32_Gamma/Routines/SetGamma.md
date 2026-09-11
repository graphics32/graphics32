---
layout: doc
docType: api
unit: GR32_Gamma
entity: SetGamma
kind: Procedure
summary: "Calculates global or custom power-law gamma lookup tables and notifies registered change listeners."
overloads:
  - signature: "procedure SetGamma; overload;"
    summary: "Recalculated global gamma encoding and decoding tables using DEFAULT_GAMMA (1.6)."
  - signature: "procedure SetGamma(Gamma: Double); overload;"
    summary: "Recalculates global gamma encoding and decoding tables using a specified power-law gamma exponent."
    parameters:
      - name: Gamma
        type: Double
        description: "Power-law gamma exponent (e.g. 1.6, 2.2)."
  - signature: "procedure SetGamma(Gamma: Double; var GammaTable: TGammaTable8Bit); overload;"
    summary: "Populates a custom 256-byte lookup table with values calculated from a specified power-law gamma exponent."
    parameters:
      - name: Gamma
        type: Double
        description: "Power-law gamma exponent."
      - name: GammaTable
        type: TGammaTable8Bit
        description: "Target 256-byte table array to receive calculated gamma values."
seealso:
  - "[[Set_sRGB]]"
  - "[[GAMMA_VALUE]]"
  - "[[GAMMA_IS_SRGB]]"
  - "[[RegisterGammaChangeNotification]]"
---

## Description

`SetGamma` calculates power-law gamma transformation values for 8-bit color channels ($0 \dots 255$).

* The single-value and parameterless overloads populate the global [[GAMMA_ENCODING_TABLE]] (using exponent $1/\gamma$) and [[GAMMA_DECODING_TABLE]] (using exponent $\gamma$), update [[GAMMA_VALUE]], set [[GAMMA_IS_SRGB]] to `False`, and invoke delegates registered with [[RegisterGammaChangeNotification]].
* The 3-parameter overload populates a custom [[TGammaTable8Bit]] array without modifying global state or firing change notifications.

## Formula

For each entry $i \in [0 \dots 255]$:

$$\text{GammaTable}[i] = \text{Round}\left(255 \cdot \left(\frac{i}{255}\right)^\gamma\right)$$

## Example

```pascal
// Set global power-law gamma to standard display gamma 2.2
SetGamma(2.2);

// Or generate a custom lookup table for custom image processing
var
  CustomTable: TGammaTable8Bit;
begin
  SetGamma(1.8, CustomTable);
  ApplyCustomGamma(MyBitmap, CustomTable);
end;
```

---
layout: doc
docType: api
unit: GR32_Math
entity: FloatMod
kind: Function
summary: "Computes floating-point modulo using floored division."
overloads:
  - signature: "function FloatMod(ANumerator, ADenominator: Double): Double; overload;"
    summary: "Computes FloatMod for double-precision floating-point values."
    parameters:
      - name: ANumerator
        type: Double
        description: "Numerator."
      - name: ADenominator
        type: Double
        description: "Denominator."
    returns:
      - type: Double
        description: "Remainder in [0..ADenominator) range."

  - signature: "function FloatMod(ANumerator, ADenominator: TFloat): TFloat; overload;"
    summary: "Computes FloatMod for single-precision floating-point values."
    parameters:
      - name: ANumerator
        type: TFloat
        description: "Numerator."
      - name: ADenominator
        type: TFloat
        description: "Denominator."
    returns:
      - type: TFloat
        description: "Remainder in [0..ADenominator) range."
seealso:
  - "[[FMod]]"
  - "[[FloatRemainder]]"
---

## Description

`FloatMod` computes floating-point modulo using the floored division definition:
$\text{Result} = \text{ANumerator} - \text{ADenominator} \cdot \text{Floor}(\text{ANumerator} / \text{ADenominator})$.

Unlike `FMod`, `FloatMod` wraps negative numerators smoothly into positive remainders.

![](/images/plot-floatmod.svg)
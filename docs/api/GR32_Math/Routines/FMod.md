---
layout: doc
docType: api
unit: GR32_Math
entity: FMod
kind: Function
summary: "Computes floating-point modulo using truncating division."
overloads:
  - signature: "function FMod(ANumerator, ADenominator: Double): Double; overload;"
    summary: "Computes FMod for double-precision floating-point values."
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

  - signature: "function FMod(ANumerator, ADenominator: TFloat): TFloat; overload;"
    summary: "Computes FMod for single-precision floating-point values."
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
  - "[[FloatMod]]"
  - "[[FloatRemainder]]"
---

## Description

`FMod` computes floating-point modulo using the standard truncating division definition:
$\text{Result} = \text{ANumerator} - \text{ADenominator} \cdot \text{Trunc}(\text{ANumerator} / \text{ADenominator})$.

![](/images/plot-fmod.svg)
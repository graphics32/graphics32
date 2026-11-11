---
layout: doc
docType: api
unit: GR32_Math
entity: FloatRemainder
kind: Function
summary: "Computes floating-point remainder using rounded division."
overloads:
  - signature: "function FloatRemainder(ANumerator, ADenominator: Double): Double; overload;"
    summary: "Computes FloatRemainder for double-precision floating-point values."
    parameters:
      - name: ANumerator
        type: Double
        description: "Numerator."
      - name: ADenominator
        type: Double
        description: "Denominator."
    returns:
      - type: Double
        description: "Remainder after rounded division."

  - signature: "function FloatRemainder(ANumerator, ADenominator: TFloat): TFloat; overload;"
    summary: "Computes FloatRemainder for single-precision floating-point values."
    parameters:
      - name: ANumerator
        type: TFloat
        description: "Numerator."
      - name: ADenominator
        type: TFloat
        description: "Denominator."
    returns:
      - type: TFloat
        description: "Remainder after rounded division."
seealso:
  - "[[FMod]]"
  - "[[FloatMod]]"
---

## Description

`FloatRemainder` computes floating-point remainder using the rounded division definition:
$\text{Result} = \text{ANumerator} - \text{ADenominator} \cdot \text{Round}(\text{ANumerator} / \text{ADenominator})$.

This matches the behavior of the standard C++ `remainder()` function.

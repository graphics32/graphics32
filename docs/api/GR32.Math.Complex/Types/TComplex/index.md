---
layout: doc
docType: api
unit: GR32.Math.Complex
entity: TComplex
kind: Type
summary: "64-bit double-precision complex number record type supporting polar coordinates, operator overloads, mathematical functions, zero-defuzzing, and string parsing."
declaration: |
  type
    TComplex = record
    public
      var Real: Double;
      var Imaginary: Double;

      class var Symbol: string;                 // defaults to 'i'
      class var SymbolBeforeImaginary: Boolean; // defaults to false
      class var DefuzzAtZero: Boolean;          // defaults to true
    end;
---

## Description

`TComplex` is a 64-bit double-precision complex record representing numbers in the complex plane $\mathbb{C}$. Each instance stores a real component (`Real`) and an imaginary component (`Imaginary`).

The structure provides:
- **Polar Coordinates**: Construction and conversion via modulus ($r$) and phase ($\theta$).
- **Operator Overloads**: Full arithmetic ($+$, $-$, $\times$, $\div$), comparison ($=$, $<>$, $<$, $<=$, $>$, $>=$), implicit, and explicit conversion operators.
- **Transcendental & Elementary Math**: Comprehensive complex trigonometric, inverse trigonometric, hyperbolic, inverse hyperbolic, exponential, logarithmic, square root, power, and sign functions.
- **Zero Defuzzing**: Configurable automatic zero-snapping via `DefuzzAtZero` to eliminate floating-point precision artifacts near zero.
- **String Parsing & Formatting**: Culture-aware formatting and parsing supporting custom imaginary symbols (e.g., `'i'` or `'j'`) and symbol placement formats.

---

## Fields Overview

| Field | Type | Scope | Description |
| --- | --- | --- | --- |
| `Real` | `Double` | Instance Field | Real scalar component ($a$ in $a + bi$). |
| `Imaginary` | `Double` | Instance Field | Imaginary scalar component ($b$ in $a + bi$). |
| `Symbol` | `string` | Class Variable | String token used for the imaginary unit during string formatting and parsing (defaults to `'i'`). |
| `SymbolBeforeImaginary` | `Boolean` | Class Variable | Determines symbol placement during string formatting (`True` for `3 + i4`, `False` for `3 + 4i`). |
| `DefuzzAtZero` | `Boolean` | Class Variable | Enables automatic zero-snapping for near-zero real or imaginary components upon construction. |

---

## Member Topics

- **[[TComplex Operators|Operators]]**: Full table of overloaded arithmetic, comparison, implicit/explicit conversion, and rounding operators.
- **[[TComplex Mathematical Functions|Mathematical functions]]**: Reference tables for polar transformations, exponential, logarithmic, power, trigonometric, hyperbolic, inverse, and string parsing/formatting functions.

---

[members]

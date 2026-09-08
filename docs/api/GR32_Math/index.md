---
layout: doc
docType: api
unit: GR32_Math
entity: GR32_Math
kind: Unit
summary: "Provides fixed-point mathematics, fast approximation algorithms, floating-point modulo routines, trigonometry helpers, and prefix-sum operations."
---

## Description

The `GR32_Math` unit contains general mathematical utilities optimized for graphics programming and high-performance calculations in Graphics32.

Key features include:
- **Fixed-Point Arithmetic**: Fast floor, ceiling, rounding, multiplication, division, reciprocal (`OneOver`), square, square root (`FixedSqrtLP`, `FixedSqrtHP`), and linear interpolation (`FixedCombine`) for `TFixed` [16:16] fixed-point numbers.
- **Trigonometry & Fast Approximations**: Multi-signature `SinCos` and `Hypot` routines, as well as fast floating-point square root (`FastSqrt`, `FastSqrtBab1`, `FastSqrtBab2`) and inverse square root (`FastInvSqrt`) approximations.
- **Modulo & Remainder Functions**: High-precision floating-point modulo operations including `FMod` (truncating division modulo), `FloatMod` (floored division modulo), and `FloatRemainder` (rounded division remainder).
- **Miscellaneous Math**: Bitwise power-of-two tests and rounding (`IsPowerOf2`, `PrevPowerOf2`, `NextPowerOf2`), integer `MulDiv`, `DivMod`, non-overflow integer `Average`, and fast `Sign`.
- **Cumulative Sum / Prefix Sum**: CPU-optimized `CumSum` routine for prefix-sum arrays.

[members]

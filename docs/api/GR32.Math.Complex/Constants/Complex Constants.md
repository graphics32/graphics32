---
layout: doc
docType: api
unit: GR32.Math.Complex
entity: Complex Constants
kind: Constant
summary: "Predefined standard complex constants for common numerical, mathematical, and sentinel values."
aliases: [ComplexOne, ComplexMinusOne, ComplexImaginaryOne, ComplexImaginaryMinusOne, ComplexHalfPi, ComplexZero, ComplexInfinity, ComplexPositiveInfinity, ComplexNegativeInfinity]
---

## Description

The `GR32.Math.Complex` unit defines a set of standard [[TComplex]] constant values representing real unit values, imaginary unit values, mathematical angle constants, zero, and infinite/NaN sentinel states.

---

## Constants Table

| Constant | Value (`Real`, `Imaginary`) | Description |
| --- | --- | --- |
| `ComplexOne` | `(1.0, 0.0)` | Real identity constant $1 + 0i$. |
| `ComplexMinusOne` | `(-1.0, 0.0)` | Negative real constant $-1 + 0i$. |
| `ComplexImaginaryOne` | `(0.0, 1.0)` | Positive imaginary unit $i = 0 + 1i$. |
| `ComplexImaginaryMinusOne` | `(0.0, -1.0)` | Negative imaginary unit $-i = 0 - 1i$. |
| `ComplexHalfPi` | `(\pi/2, 0.0)` | Real constant representing $\frac{\pi}{2}$ radians ($90^\circ$). |
| `ComplexZero` | `(0.0, 0.0)` | Complex zero $0 + 0i$. |
| `ComplexInfinity` | `(NaN, NaN)` | Sentinel complex infinity with both real and imaginary components set to `NaN`. |
| `ComplexPositiveInfinity` | `(+Infinity, 0.0)` | Real positive infinity $+\infty + 0i$. |
| `ComplexNegativeInfinity` | `(-Infinity, 0.0)` | Real negative infinity $-\infty + 0i$. |

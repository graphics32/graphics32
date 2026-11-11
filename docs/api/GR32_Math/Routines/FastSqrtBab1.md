---
layout: doc
docType: api
unit: GR32_Math
entity: FastSqrtBab1
kind: Function
declaration: "function FastSqrtBab1(const Value: TFloat): TFloat;"
summary: "Computes a fast square root approximation refined with one Babylonian iteration step."
parameters:
  - name: Value
    type: TFloat
    description: "Input floating-point value."
returns:
  - type: TFloat
    description: "Approximated square root."
seealso:
  - "[[FastSqrt]]"
  - "[[FastSqrtBab2]]"
  - "[Babylonian method - Wikipedia](https://en.wikipedia.org/wiki/Square_root_algorithms#Heron's_method)"
---

## Description

`FastSqrtBab1` computes a fast single-precision square root approximation using bit manipulation refined with one Babylonian method iteration step for enhanced precision.

## Algorithm Details

The algorithm combines direct IEEE 754 floating-point bit manipulation with one step of Heron's (Babylonian) method:

1. **Initial Bitwise Seed**:
   The floating-point exponent bits are manipulated directly to produce an initial fast guess $x_0$:
   $$x_0 = \text{BitCastToFloat}\left(\frac{\text{BitCastToInt}(Value) - 0x3F800000}{2} + 0x3F800000\right)$$
2. **Babylonian Refinement Step**:
   A single Newton-Raphson / Babylonian iteration step is applied to refine accuracy:
   $$x_1 = 0.5 \cdot \left(x_0 + \frac{Value}{x_0}\right)$$

This provides significantly greater numerical accuracy than `FastSqrt` while remaining substantially faster than standard math library functions or FPU square root routines on older instruction sets.

---
layout: doc
docType: api
unit: GR32_LowLevel
entity: GR32_LowLevel
kind: Unit
summary: "Provides low-level memory operations, fast mathematical routines, bit manipulations, coordinate wrapping, and optimized rounding operations."
---

## Description

The `GR32_LowLevel` unit contains performance-critical low-level primitives used extensively across Graphics32. It provides optimized memory fills and copies, fast division, fast arithmetic shifts, range clamping and wrapping functions, and accelerated fast-rounding routines (utilizing CPU instruction set extensions such as SSE2 and SSE4.1 where available).

Key capabilities provided by this unit include:

- **Optimized Memory Operations**: 32-bit and 16-bit array filling and copying routines ([[FillLongword]], [[FillWord]], [[MoveLongword]], [[MoveWord]]).
- **Stack Memory Allocation**: Stack-based dynamic memory allocation primitives ([[StackAlloc]], [[StackFree]]).
- **Fast Value Manipulation**: Swapping operations ([[Swap]], [[Swap32]], [[Swap16]], [[Swap64]]), conditional swapping ([[TestSwap]]), and range clipping check ([[TestClip]]).
- **Boundary Clamping, Wrapping & Reflection**: Flexible coordinate mapping functions including [[Clamp]], [[Constrain]], [[SwapConstrain]], [[Wrap]], [[WrapMem]], [[WrapPow2]], [[Mirror]], [[Reflect]], and [[ReflectPow2]].
- **Optimal Wrap Function Resolvers**: [[GetOptimalWrap]], [[GetOptimalReflect]], [[GetWrapProc]], and [[GetWrapProcEx]] for dynamically selecting optimized wrapping procedure delegates.
- **Fast Integer Division**: Accelerated division by 255 and 127 ([[Div255]], [[FastDiv255]], [[Div255Round]], [[Div127]]).
- **Fast Rounding Functions**: High-performance replacement for RTL rounding and truncation routines ([[Fast Rounding Functions|FastFloor, FastCeil, FastTrunc, FastRound]]).
- **Arithmetic Right Shifts**: Sign-preserving bitwise right shifts ([[SAR_3]] through [[SAR_16]]).
- **Color Processing**: Color format conversion between VCL/WinAPI `TColor` and 32-bit `TColor32` via [[ColorSwap]].

---

[members]

---
layout: doc
docType: api
unit: GR32_Blend
entity: GR32_Blend
kind: Unit
summary: "Provides core alpha blending, compositing, linear combination, color algebra, premultiplication, and optimized lookup tables for 32-bit ARGB pixel operations."
seealso:
  - "[Alpha Composition](/guide/alpha-composition)"
---

## Description

The `GR32_Blend` unit serves as the primary low-level color compositing engine in Graphics32. It provides high-performance routines for alpha blending, associative alpha merging, linear interpolation, color channel algebra, brightness adjustments, and alpha premultiplication/unpremultiplication.

Key capabilities provided by `GR32_Blend` include:

- **Alpha Blending**: Mixes a foreground color $F$ onto an opaque background color $B$ using foreground alpha $F_a$:
  $$Z = F_a \cdot F_{rgb} + (1 - F_a) \cdot B_{rgb}$$
- **Associative Alpha Merging**: Combines foreground $F$ and semi-transparent background $B$ using Bruce Wallace's associative compositing formula:
  $$A \text{ over } (B \text{ over } C) = (A \text{ over } B) \text{ over } C$$
- **Linear Combination (Combine / Lerp)**: Performs linear interpolation between two 32-bit colors given a weight $W \in [0, 255]$:
  $$Z = W \cdot X + (1 - W) \cdot Y$$
- **Color Algebra & Blend Modes**: Includes standard color blend operations such as Add, Subtract, Divide, Modulate, Max, Min, Difference, Average, Exclusion, Scale, Screen, Dodge, Burn, BlendColorAdd, and BlendColorModulate.
- **Alpha Premultiplication & Unpremultiplication**: Conversions between straight ARGB and premultiplied ARGB color formats (`Premultiply32`, `Unpremultiply32`, `PremultiplyMem`, `UnpremultiplyMem`).
- **Optimized Lookup Tables**: Precalculated $256 \times 256$ lookup tables (`MulDiv255Table`, `DivMul255Table`) and SIMD alignment tables (`alpha_ptr`, `bias_ptr`) to eliminate runtime division.
- **Dynamic Hardware Bindings**: Function delegates (`BlendReg`, `BlendMem`, `BlendMems`, etc.) dynamically bound to pure Pascal, x86/x64 assembly, or SSE2 vector implementations at runtime based on CPU feature detection.

---

[members]

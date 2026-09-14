---
layout: doc
docType: api
unit: GR32_Blend
entity: Combine
aliases: [CombineReg, CombineMem, CombineLine]
kind: Function
declaration: |
  function CombineReg(F, B: TColor32): TColor32;
  procedure CombineMem(F: TColor32; var B: TColor32);
  procedure CombineLine(Src, Dst: PColor32; Count: Integer);
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
  - name: Src
    type: PColor32
    description: "Pointer to the first source foreground pixel in memory."
  - name: Dst
    type: PColor32
    description: "Pointer to the first destination background pixel in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous background pixels to blend."
returns:
  - type: TColor32
    description: "The blended 32-bit ARGB result color."
summary: "Linear interpolation (Lerp) combining two colors across all channels given a weight."
seealso:
  - "[[BlendReg]]"
  - "[[MergeReg]]"
  - "[Alpha Composition](/guide/alpha-composition)"
  - "[Naming Conventions](/guide/naming-conventions)"
---

## Description

The `Combine*` family of functions performs linear interpolation (Lerp) between two 32-bit ARGB colors ($X$ and $Y$) given weight $W \in [0, 255]$:

$$Z = \frac{W}{255} \cdot X + \left(1 - \frac{W}{255}\right) \cdot Y = \frac{W \cdot (X - Y)}{255} + Y$$

All four channels (Alpha, Red, Green, Blue) are combined simultaneously.

### Variant Summary

| Variant | Signature | Description |
| --- | --- | --- |
| `CombineReg` | `function(X, Y: TColor32; W: Cardinal): TColor32;` | Combines colors $X$ and $Y$ in registers with weight $W$. |
| `CombineMem` | `procedure(X: TColor32; var Y: TColor32; W: Cardinal);` | Combines color $X$ into memory location $Y$ in-place with weight $W$. |
| `CombineLine` | `procedure(Src, Dst: PColor32; Count: Integer; W: Cardinal);` | Combines `Count` pixels between `Src` and `Dst` scanline buffers with weight $W$. |

::: info
All these functions are actually delegates; At startup, Graphics32 [binds them to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
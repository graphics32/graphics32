---
layout: doc
docType: api
unit: GR32_Blend
entity: Merge
aliases: [MergeReg, MergeMem, MergeMems, MergeRegEx, MergeMemEx, MergeLine, MergeLineEx]
kind: Function
declaration: |
  function MergeReg(F, B: TColor32): TColor32;
  procedure MergeMem(F: TColor32; var B: TColor32);
  procedure MergeMems(F: TColor32; Dst: PColor32; Count: Integer);
  function MergeRegEx(F, B: TColor32; M: Cardinal): TColor32;
  procedure MergeMemEx(F: TColor32; var B: TColor32; M: Cardinal);
  procedure MergeLine(Src, Dst: PColor32; Count: Integer);
  procedure MergeLineEx(Src, Dst: PColor32; Count: Integer; M: Cardinal);
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
  - name: M
    type: Cardinal
    description: "Master alpha weight or mask value (0..255)."
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
summary: "Associative alpha merging taking both foreground and background alpha channels into account."
seealso:
  - "[[Blend]]"
  - "[[Combine]]"
  - "[Alpha Composition](/guide/alpha-composition)"
  - "[Naming Conventions](/guide/naming-conventions)"
---

## Description

The `Merge*` family of functions performs associative alpha merging based on Bruce Wallace's 1981 compositing formulation:

$$R_a = F_a + B_a \cdot (1 - F_a)$$
$$R_c = B_c + \frac{F_a}{R_a} \cdot (F_c - B_c)$$

Unlike standard alpha blending ([[Blend]]), `Merge` handles semi-transparent background colors accurately, maintaining associativity:
$$(A \text{ over } B) \text{ over } C = A \text{ over } (B \text{ over } C)$$

### Variant Summary

| Variant | Signature | Description |
| --- | --- | --- |
| `MergeReg` | `function(F, B: TColor32): TColor32;` | Merges foreground $F$ and background $B$ in registers. |
| `MergeMem` | `procedure(F: TColor32; var B: TColor32);` | Merges foreground $F$ directly into memory location $B$. |
| `MergeMems` | `procedure(F: TColor32; B: PColor32; Count: Integer);` | Merges uniform foreground $F$ into `Count` background pixels starting at $B$. |
| `MergeRegEx` | `function(F, B: TColor32; M: Cardinal): TColor32;` | Register merging with master alpha / mask factor $M$ ($0 \dots 255$). |
| `MergeMemEx` | `procedure(F: TColor32; var B: TColor32; M: Cardinal);` | In-memory merging with master alpha / mask factor $M$ ($0 \dots 255$). |
| `MergeLine` | `procedure(Src, Dst: PColor32; Count: Integer);` | Scanline merging of `Count` source pixels onto destination pixels. |
| `MergeLineEx` | `procedure(Src, Dst: PColor32; Count: Integer; M: Cardinal);` | Scanline merging with master alpha factor $M$. |


::: info
All these functions are actually delegates; At startup, Graphics32 [binds them to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
---
layout: doc
docType: api
unit: GR32_Blend
entity: Blend
aliases: [BlendReg, BlendMem, BlendMems, BlendRegEx, BlendMemEx, BlendRegRGB, BlendMemRGB, BlendLine, BlendLineEx]
kind: Function
declaration: |
  function BlendReg(F, B: TColor32): TColor32;
  procedure BlendMem(F: TColor32; var B: TColor32);
  procedure BlendMems(F: TColor32; Dst: PColor32; Count: Integer);
  function BlendRegEx(F, B: TColor32; M: Cardinal): TColor32;
  procedure BlendMemEx(F: TColor32; var B: TColor32; M: Cardinal);
  function BlendRegRGB(F, B: TColor32; W: Cardinal): TColor32;
  procedure BlendMemRGB(F: TColor32; var B: TColor32; W: Cardinal);
  procedure BlendLine(Src, Dst: PColor32; Count: Integer);
  procedure BlendLineEx(Src, Dst: PColor32; Count: Integer; M: Cardinal);
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
  - name: W
    type: Cardinal
    description: "Explicit blending weight (0..255)."
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
summary: "Alpha blending for single pixels, array memory buffers, and scanlines."
seealso:
  - "[[Merge]]"
  - "[[Combine]]"
  - "[Alpha Composition](/guide/alpha-composition)"
  - "[Naming Conventions](/guide/naming-conventions)"
---

## Description

The `Blend*` family of functions performs standard alpha blending, compositing a foreground color $F$ onto a background color $B$ using $F$'s alpha channel value $F_a$:

$$Z_{rgb} = F_a \cdot F_{rgb} + (1 - F_a) \cdot B_{rgb}$$

The background color $B$ is assumed to be fully opaque ($B_a = 255$). If $B$ contains semi-transparent alpha, use [[Merge]] instead.

### Variant Summary

| Variant | Signature | Description |
| --- | --- | --- |
| `BlendReg` | `function(F, B: TColor32): TColor32;` | Blends foreground $F$ and background $B$ in registers. |
| `BlendMem` | `procedure(F: TColor32; var B: TColor32);` | Blends foreground $F$ directly into memory location $B$. |
| `BlendMems` | `procedure(F: TColor32; B: PColor32; Count: Integer);` | Blends uniform foreground $F$ into `Count` background pixels starting at $B$. |
| `BlendRegEx` | `function(F, B: TColor32; M: Cardinal): TColor32;` | Register blending with master alpha / mask factor $M$ ($0 \dots 255$). |
| `BlendMemEx` | `procedure(F: TColor32; var B: TColor32; M: Cardinal);` | In-memory blending with master alpha / mask factor $M$ ($0 \dots 255$). |
| `BlendRegRGB` | `function(F, B: TColor32; W: Cardinal): TColor32;` | Register blending with explicit blend weight $W$ ($0 \dots 255$). |
| `BlendMemRGB` | `procedure(F: TColor32; var B: TColor32; W: Cardinal);` | In-memory blending with explicit blend weight $W$ ($0 \dots 255$). |
| `BlendLine` | `procedure(Src, Dst: PColor32; Count: Integer);` | Scanline blending of `Count` source pixels onto destination pixels. |
| `BlendLineEx` | `procedure(Src, Dst: PColor32; Count: Integer; M: Cardinal);` | Scanline blending with master alpha factor $M$. |

::: info
All these functions are actually delegates; At startup, Graphics32 [binds them to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
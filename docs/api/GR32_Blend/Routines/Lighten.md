---
layout: doc
docType: api
unit: GR32_Blend
entity: LightenReg
aliases: [Lighten]
kind: Function
declaration: |
  function LightenReg(F, B: TColor32): TColor32;
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color."
returns:
  - type: TColor32
    description: "The blended 32-bit ARGB result color."
summary: "Pixel brightness adjustment."
seealso:
  - "[[ScaleMems]]"
---

## Description

`LightenReg` (aliased as `Lighten`) adjusts the brightness of pixel color $C$ by adding an offset `Amount` (ranging from $-255$ to $+255$) to RGB components, clamping results to $[0, 255]$. The alpha channel remains unchanged.

::: info
This function is actually a delegate; At startup, Graphics32 [binds it to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
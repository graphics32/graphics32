---
layout: doc
docType: api
unit: GR32_Blend
entity: ScaleMems
kind: Function
declaration: |
  procedure ScaleMems(Dst: PColor32; Count: Integer; Weight: Cardinal);
parameters:
  - name: Dst
    type: PColor32
    description: "Pointer to the first pixel in memory."
  - name: Count
    type: Integer
    description: "Number of contiguous pixels to scale."
  - name: Weight
    type: Cardinal
    description: "Scale factor (0..255)."
summary: "RGB component scaling."
seealso:
  - "[[LightenReg]]"
---

## Description

`ScaleMems` scales the RGB components of `Count` contiguous pixels starting at `Dst` by `Weight` factor ($0 \dots 255$).

::: info
This function is actually a delegate; At startup, Graphics32 [binds it to the optimal implementation](/guide/cpu-feature-detection) (Pure Pascal, x86/x64 assembly, or SSE2 vector instructions) supported by the host CPU.
:::
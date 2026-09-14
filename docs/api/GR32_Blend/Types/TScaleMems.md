---
layout: doc
docType: api
unit: GR32_Blend
entity: TScaleMems
kind: Type
declaration: "type TScaleMems = procedure(Dst: PColor32; Count: Integer; Weight: Cardinal);"
summary: "Procedural delegate for scaling RGB components of a pixel buffer by a constant factor."
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
seealso:
  - "[[ScaleMems]]"
---

## Description

`TScaleMems` defines the signature for scaling RGB components of `Count` contiguous pixels starting at `Dst` by a constant weight factor `Weight` ($0 \dots 255$).

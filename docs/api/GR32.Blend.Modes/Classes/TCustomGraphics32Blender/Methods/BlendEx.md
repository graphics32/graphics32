---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TCustomGraphics32Blender
entity: TCustomGraphics32Blender.BlendEx
kind: Method
declaration: "procedure BlendEx(F: TColor32; var B: TColor32; M: Cardinal); virtual;"
summary: "Blends foreground color F onto background color B with packed master alpha values for both source and destination."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Background pixel color variable updated in-place with the blended result."
  - name: M
    type: Cardinal
    description: "Packed master alpha value where lower 8 bits contain foreground master alpha and next 8 bits contain background master alpha."
---

## Description

`BlendEx` provides extended blending where master alpha modulation is applied to both foreground and background channels prior to blending. The `M` parameter packs both values: bits 0..7 contain the foreground master alpha, while bits 8..15 contain the background master alpha.

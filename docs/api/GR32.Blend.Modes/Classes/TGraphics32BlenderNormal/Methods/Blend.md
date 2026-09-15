---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TGraphics32BlenderNormal
entity: TGraphics32BlenderNormal.Blend
kind: Method
summary: "Blends foreground color F onto background color B using standard alpha blending."
overloads:
  - signature: "function Blend(F: TColor32; B: TColor32): TColor32; override;"
    summary: "Blends foreground pixel F onto background pixel B using MergeReg."
    parameters:
      - name: F
        type: TColor32
        description: "Foreground pixel color."
      - name: B
        type: TColor32
        description: "Background pixel color."
    returns:
      type: TColor32
      description: "The resulting blended 32-bit ARGB color."

  - signature: "procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); override;"
    summary: "Blends foreground pixel F onto background pixel variable B with master alpha M using MergeMemEx."
    parameters:
      - name: F
        type: TColor32
        description: "Foreground pixel color."
      - name: B
        type: TColor32
        description: "Background pixel color variable updated in-place."
      - name: M
        type: Cardinal
        description: "Master alpha modulation value."
---

## Description

`Blend` overrides `TCustomGraphics32Blender.Blend` to invoke core Graphics32 routines (`MergeReg` for pixel functions and `MergeMemEx` for memory-destination routines).

---
layout: doc
docType: api
unit: GR32.Blend.Modes
parent: TCustomGraphics32Blender
entity: TCustomGraphics32Blender.Blend
kind: Method
summary: "Blends a foreground color onto a background color."
overloads:
  - signature: "function Blend(F: TColor32; B: TColor32): TColor32; overload; virtual; abstract;"
    summary: "Blends foreground color F onto background color B and returns the resulting 32-bit ARGB color."
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

  - signature: "procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); overload; virtual;"
    summary: "Blends foreground color F onto background color variable B with master alpha modulation M."
    parameters:
      - name: F
        type: TColor32
        description: "Foreground pixel color."
      - name: B
        type: TColor32
        description: "Background pixel color variable updated in-place with the blended result."
      - name: M
        type: Cardinal
        description: "Master alpha value (0..255) used to modulate the foreground alpha."
---

## Description

`Blend` computes the color combination of foreground pixel `F` and background pixel `B`. The function overload returns the resulting blended `TColor32` value, while the procedure overload applies master alpha modulation `M` to the foreground pixel's alpha component and updates the background color variable `B` in-place.

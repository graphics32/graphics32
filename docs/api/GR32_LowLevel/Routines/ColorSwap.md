---
layout: doc
docType: api
unit: GR32_LowLevel
entity: ColorSwap
kind: Function
declaration: "function ColorSwap(WinColor: TColor): TColor32;"
summary: "Swaps the R and B color channel and sets alpha to 255."
parameters:
  - name: WinColor
    type: TColor
    description: "Color value to convert."
returns:
  - type: TColor32
    description: "The color value with red and blue channels swapped and alpha channel set to $FF."
seealso:
  - "[[Color32]]"
---

## Description

`ColorSwap` exchanges the red and blue color components and forces the alpha channel component to `$FF` (fully opaque). It is typically used to convert between ARGB and ABGR.

::: warning
`ColorSwap` does not take the platform ARGB layout into account but simply swaps R and B, regardless on layout.
It can therefore **only** be used to convert between a Windows/VCL color and a Graphics32 TColor32, on the platforms where the ARGB layout of these two differs. For example on Windows.
:::
---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: CombineInfo
kind: Function
declaration: "function CombineInfo(Bitmap: TCustomBitmap32): TCombineInfo;"
summary: "Extracts drawing modes, master alpha, and combine callbacks from a bitmap into a TCombineInfo record."
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Source bitmap whose master alpha, draw mode, combine mode, outer color, and pixel combine event are queried."
---

## Description

The `CombineInfo` function queries drawing configuration properties from the specified `Bitmap` and constructs a corresponding [[TCombineInfo]] structure.

The returned record copies:
- `SrcAlpha` from `Bitmap.MasterAlpha`
- `DrawMode` from `Bitmap.DrawMode` (resetting `dmCustom` to `dmOpaque` if `OnPixelCombine` is unassigned)
- `CombineMode` from `Bitmap.CombineMode`
- `CombineCallBack` from `Bitmap.OnPixelCombine`
- `TransparentColor` from `Bitmap.OuterColor`

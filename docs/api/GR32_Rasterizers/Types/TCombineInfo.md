---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TCombineInfo
kind: Type
aliases: [PCombineInfo]
declaration: |
  PCombineInfo = ^TCombineInfo;
  TCombineInfo = record
    SrcAlpha: Integer;
    DrawMode: TDrawMode;
    CombineMode: TCombineMode;
    CombineCallBack: TPixelCombineEvent;
    TransparentColor: TColor32;
  end;
summary: "Record structure holding pixel combination parameters, alpha transparency, drawing mode, and combine callbacks for rasterization."
---

## Description

`TCombineInfo` encapsulates combination parameters passed to [[TRasterizer.Rasterize]] methods. It configures how sampled pixel colors from a [[TCustomSampler]] are blended into target bitmap pixels.

| Field | Type | Description |
| --- | --- | --- |
| `SrcAlpha` | Integer | Master alpha transparency multiplier (0–255) applied during blending. |
| `DrawMode` | TDrawMode | Drawing mode specifying pixel combination logic (`dmOpaque`, `dmBlend`, `dmTransparent`, or `dmCustom`). |
| `CombineMode` | TCombineMode | Alpha combine mode selected when `DrawMode` is `dmBlend` (`cmBlend`, `cmMerge`, etc.). |
| `CombineCallBack` | TPixelCombineEvent | Custom pixel combination callback procedure executed when `DrawMode` is `dmCustom`. |
| `TransparentColor` | TColor32 | Transparent key color ignored during rasterization when `DrawMode` is `dmTransparent`. |

Use the [[CombineInfo]] function to initialize a `TCombineInfo` structure automatically from an existing [[TCustomBitmap32]] instance.

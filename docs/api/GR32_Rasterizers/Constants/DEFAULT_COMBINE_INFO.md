---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: DEFAULT_COMBINE_INFO
kind: Constant
declaration: |
  const DEFAULT_COMBINE_INFO: TCombineInfo = (
    SrcAlpha: $FF;
    DrawMode: dmOpaque;
    CombineMode: cmBlend;
    CombineCallBack: nil;
    TransparentColor: clBlack32;
  );
summary: "Default pixel combination settings with opaque drawing mode and full alpha transparency."
---

## Description

`DEFAULT_COMBINE_INFO` defines default rasterizer pixel combination parameters:
- `SrcAlpha`: `$FF` (255, completely opaque)
- `DrawMode`: `dmOpaque`
- `CombineMode`: `cmBlend`
- `CombineCallBack`: `nil`
- `TransparentColor`: `clBlack32`

This constant is used to initialize new [[TRasterizer]] instances upon creation.

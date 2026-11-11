---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.OnPixelCombine
kind: Event
scope: Published
declaration: "property OnPixelCombine: TPixelCombineEvent read FOnPixelCombine write FOnPixelCombine;"
summary: "Custom pixel combination event fired when DrawMode is set to dmCustom."
---

## Description

`OnPixelCombine` allows custom pixel blending logic when `DrawMode = dmCustom`.

## Signature

```pascal
type TPixelCombineEvent = procedure(F: TColor32; var B: TColor32; M: Cardinal) of object;
```

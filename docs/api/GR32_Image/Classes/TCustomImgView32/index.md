---
layout: doc
docType: api
unit: GR32_Image
entity: TCustomImgView32
kind: Class
abstract: true
declaration: "TCustomImgView32 = class(TCustomImage32);"
inheritance:
  - TGraphics32ControlBaseClass
  - TCustomPaintBox32
  - TCustomImage32
  - TCustomImgView32
summary: "Base class for image view controls featuring integrated scrollbars, automatic centering, and size grip."
seealso:
  - "[[TImgView32]]"
  - "[[TCustomImage32]]"
---

## Description

`TCustomImgView32` extends `TCustomImage32` with horizontal (`HScroll`) and vertical (`VScroll`) scrollbars, centering modes (`Centered`), and size grip rendering (`SizeGrip`).

[members]

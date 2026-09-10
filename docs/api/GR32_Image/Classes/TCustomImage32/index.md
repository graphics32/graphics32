---
layout: doc
docType: api
unit: GR32_Image
entity: TCustomImage32
kind: Class
abstract: true
declaration: "TCustomImage32 = class(TCustomPaintBox32, IUpdateRectNotification, ILayerListNotification);"
inheritance:
  - TGraphics32ControlBaseClass
  - TCustomPaintBox32
  - TCustomImage32
summary: "Base class for interactive 32-bit bitmap viewport controls supporting alpha blending, layers, custom backgrounds, panning, and zooming."
seealso:
  - "[[TImage32]]"
  - "[[TCustomImgView32]]"
---

## Description

`TCustomImage32` extends `TCustomPaintBox32` with comprehensive 32-bit bitmap displaying capabilities. It manages a `TBitmap32` instance (`Bitmap`), a `TLayerCollection` (`Layers`), customizable `PaintStages`, background decoration options (`Background`), interactive mouse panning (`MousePan`), and wheel zooming (`MouseZoom`).

**Implements**
| Interface | Description |
|---|---|
| `IUpdateRectNotification` | Receives area update invalidation notifications. |
| `ILayerListNotification` | Receives layer collection state change notifications. |

[members]

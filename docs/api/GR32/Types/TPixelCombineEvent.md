---
layout: doc
docType: api
unit: GR32
entity: TPixelCombineEvent
kind: Type
declaration: "type TPixelCombineEvent = procedure(F: TColor32; var B: TColor32; M: Cardinal) of object;"
summary: "Event type for custom pixel combining callbacks."
parameters:
  - name: F
    type: TColor32
    description: "Foreground pixel color."
  - name: B
    type: TColor32
    description: "Reference to the destination background pixel to be updated in-place."
  - name: M
    type: Cardinal
    description: "Master alpha weight or mask value (0..255)."
seealso:
  - "[[TCustomBitmap32.OnPixelCombine]]"
  - "[[TCustomImage32.OnBitmapPixelCombine]]"
  - "[[TBitmapPaintBrush.BlendFunc]]"
  - "[[BlockTransfer]]"
  - "[[StretchTransfer]]"
  - "[[TDrawMode]]"
---

## Description

`TPixelCombineEvent` defines the callback event signature invoked when drawing in custom blend mode (`dmCustom`).

The handler receives the foreground pixel color `F`, a reference to the destination background pixel `B`, and master alpha weight `M`. The handler calculates the resulting color and modifies `B` in-place.

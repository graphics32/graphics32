---
layout: doc
docType: api
unit: GR32_Layers
parent: TPositionedLayer
entity: TPositionedLayer.OnGetUpdateRect
kind: Event
scope: Published
declaration: "property OnGetUpdateRect: TLayerGetUpdateRectEvent read ... write ...;"
summary: "Fired when calculating the invalidated screen rectangle for the layer."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance."
  - name: UpdateRect
    type: TRect
    description: "Var parameter specifying the invalidated bounding rectangle in viewport coordinates."
---

## Description

`OnGetUpdateRect` allows customization of the screen rectangle invalidated during layer updates. The event delegate type is defined as:

```pascal
type TLayerGetUpdateRectEvent = procedure(Sender: TObject; var UpdateRect: TRect) of object;
```

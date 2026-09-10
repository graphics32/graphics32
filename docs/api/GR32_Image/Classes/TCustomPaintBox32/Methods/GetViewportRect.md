---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.GetViewportRect
kind: Method
scope: Public
declaration: "function GetViewportRect: TRect; virtual;"
summary: "Returns client rectangle available for buffer viewport display."
returns:
  - type: TRect
    description: "Viewport rectangle in local client coordinates."
---

## Description

`GetViewportRect` returns the buffered area within the control bounds. By default the whole control client area is buffered.

---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImgView32
entity: TCustomImgView32.GetViewportRect
kind: Method
declaration: "function GetViewportRect: TRect; override;"
summary: "Returns viewport rectangle excluding visible scrollbars."
returns:
  - type: TRect
    description: "Viewport rectangle in control space."
---

## Description

`GetViewportRect` returns the client area rectangle reduced by the dimensions of visible horizontal or vertical scrollbars.

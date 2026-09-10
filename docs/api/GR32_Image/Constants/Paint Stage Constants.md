---
layout: doc
docType: api
unit: GR32_Image
entity: Paint Stage Constants
kind: Constant
summary: "Defines stage identifiers used by TPaintStage and TCustomImage32 rendering pipeline."
aliases: [PST_CUSTOM, PST_CLEAR_BUFFER, PST_CLEAR_BACKGND, PST_DRAW_BITMAP, PST_DRAW_LAYERS, PST_CONTROL_FRAME, PST_BITMAP_FRAME]
declaration: |
  PST_CUSTOM        = 1;
  PST_CLEAR_BUFFER  = 2;
  PST_CLEAR_BACKGND = 3;
  PST_DRAW_BITMAP   = 4;
  PST_DRAW_LAYERS   = 5;
  PST_CONTROL_FRAME = 6;
  PST_BITMAP_FRAME  = 7;
---

## Description

The Paint Stage Constants define the build-in stage types executed during the rendering pipeline of `TCustomImage32`.

| Constant | Value | Description |
|---|---|---|
| `PST_CUSTOM` | `1` | Invokes the `OnPaintStage` event with the current stage index. |
| `PST_CLEAR_BUFFER` | `2` | Clears the entire buffer using the background color. |
| `PST_CLEAR_BACKGND` | `3` | Clears visible background areas surrounding the bitmap (checkers, borders, drop shadow, pattern). |
| `PST_DRAW_BITMAP` | `4` | Draws the bitmap surface onto the destination buffer. |
| `PST_DRAW_LAYERS` | `5` | Renders visible layers according to the layer mask parameter. |
| `PST_CONTROL_FRAME` | `6` | Draws a dotted frame around the control bounds (design-time). |
| `PST_BITMAP_FRAME` | `7` | Draws a dotted frame around the scaled bitmap bounds (design-time). |

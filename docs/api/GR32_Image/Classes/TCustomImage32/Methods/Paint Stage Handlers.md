---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.Paint Stage Handlers
kind: Method
summary: "Execution methods invoked during TCustomImage32 paint stage rendering pipeline."
aliases: [ExecBitmapFrame, ExecClearBuffer, ExecClearBackgnd, ExecControlFrame, ExecCustom, ExecDrawBitmap, ExecDrawLayers]
declaration: |
  procedure ExecBitmapFrame(Dest: TBitmap32; StageNum: Integer); virtual;
  procedure ExecClearBuffer(Dest: TBitmap32; StageNum: Integer); virtual;
  procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer); virtual;
  procedure ExecControlFrame(Dest: TBitmap32; StageNum: Integer); virtual;
  procedure ExecCustom(Dest: TBitmap32; StageNum: Integer); virtual;
  procedure ExecDrawBitmap(Dest: TBitmap32; StageNum: Integer); virtual;
  procedure ExecDrawLayers(Dest: TBitmap32; StageNum: Integer); virtual;
seealso:
  - "[[Paint Stage Constants]]"
  - "[[TPaintStages]]"
---

## Description

The `Exec*` methods are virtual paint stage execution handlers called by `TCustomImage32` during buffer rendering. Each method processes a corresponding built-in stage in `PaintStages`.

| Method | Stage Constant | Description |
|---|---|---|
| `ExecCustom` | `PST_CUSTOM` | Triggers the `OnPaintStage` event for custom drawing. |
| `ExecClearBuffer` | `PST_CLEAR_BUFFER` | Clears the destination buffer surface. |
| `ExecClearBackgnd` | `PST_CLEAR_BACKGND` | Clears background areas (solid color, checkers, pattern bitmap, borders, drop shadow) surrounding the bitmap. |
| `ExecDrawBitmap` | `PST_DRAW_BITMAP` | Renders the bitmap surface onto the destination buffer. |
| `ExecDrawLayers` | `PST_DRAW_LAYERS` | Renders visible layers matching the stage parameter mask onto the destination buffer. |
| `ExecControlFrame` | `PST_CONTROL_FRAME` | Draws a dotted focus frame around the control bounds during design-time. |
| `ExecBitmapFrame` | `PST_BITMAP_FRAME` | Draws a dotted focus frame around the scaled bitmap bounds during design-time. |

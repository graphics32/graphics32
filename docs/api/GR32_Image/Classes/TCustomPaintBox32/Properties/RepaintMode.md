---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.RepaintMode
kind: Property
aliases: [TRepaintMode, rmFull, rmDirect, rmOptimizer]
scope: Public
declaration: |
  type
    TRepaintMode = (rmFull, rmDirect, rmOptimizer);

  property RepaintMode: TRepaintMode read FRepaintMode write SetRepaintMode default rmFull;
summary: "Specifies invalidation and repaint strategy used for buffer rendering."
---

## Description

`RepaintMode` controls how updates to the internal buffer are repainted onto the screen.

| Value | Description |
| --- | --- |
| `rmFull` | Invalidates and repaints the entire control area whenever a change occurs. |
| `rmDirect` | Repaints invalidated regions directly to the screen without queuing standard OS paint messages. |
| `rmOptimizer` | Uses an internal [repaint optimizer](/guide/repaint-optimization) to coalesce invalid rectangles and minimize redrawn areas. |

---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.Flush
kind: Method
scope: Public
overloads:
  - signature: "procedure Flush;"
    summary: "Forces immediate repainting of all pending invalid regions to screen."
  - signature: "procedure Flush(const SrcRect: TRect);"
    summary: "Forces immediate repainting of a specific source sub-rectangle to screen."
    parameters:
      - name: SrcRect
        type: TRect
        description: "Sub-rectangle within the control buffer to immediately flush."
---

## Description

`Flush` forces pending invalid regions the buffer to repaint immediately onto the control's screen context.

---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.Buffer
kind: Property
scope: Public
declaration: "property Buffer: TBitmap32 read FBuffer;"
summary: "Provides access to the internal 32-bit double-buffered bitmap surface."
---

## Description

`Buffer` provides access to the underlying [[TBitmap32]] double-buffer managed by the control. The control renders onto this buffer before it is painted to screen.

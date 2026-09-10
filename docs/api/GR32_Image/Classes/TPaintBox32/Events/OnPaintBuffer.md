---
layout: doc
docType: api
unit: GR32_Image
parent: TPaintBox32
entity: TPaintBox32.OnPaintBuffer
kind: Event
scope: Published
declaration: "property OnPaintBuffer: TNotifyEvent read FOnPaintBuffer write FOnPaintBuffer;"
summary: "Fired when the double-buffered TBitmap32 surface requires repainting."
---

## Description

The `OnPaintBuffer` is fired when the control needs to repaint the buffer. Since the control is double-buffered, this only occurs when the buffer has been invalidated, either explicitly (e.g. via [[Invalidate]]), or implicitly (e.g. by a resize).

The `OnPaintBuffer` event handler should draw onto the [[Buffer]] bitmap. Upon return from the event handler, the buffer will be marked valid and painted onto the screen.
---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.OnGDIOverlay
kind: Event
scope: Public
declaration: "property OnGDIOverlay: TNotifyEvent read FOnGDIOverlay write FOnGDIOverlay;"
summary: "Fired after buffer rendering to allow drawing GDI overlays directly to control Canvas."
---

## Description

`OnGDIOverlay` allows applications to draw native platform graphics or text onto the control canvas *after* the control's double-buffer has been drawn to screen.

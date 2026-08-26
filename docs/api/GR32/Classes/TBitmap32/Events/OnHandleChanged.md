---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.OnHandleChanged
kind: Event
scope: Published
declaration: "property OnHandleChanged: TNotifyEvent read FOnHandleChanged write FOnHandleChanged;"
summary: "Fired when the OS GDI bitmap surface handle (Handle) is changed or re-allocated."
---

## Description

`OnHandleChanged` fires when the backend surface handle is recreated or updated (for instance, following a surface resize or backend change).

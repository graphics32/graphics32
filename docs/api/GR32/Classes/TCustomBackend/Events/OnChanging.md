---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.OnChanging
kind: Event
scope: Published
declaration: "property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;"
summary: "Fired immediately before backend surface dimensions or memory buffers change."
---

## Description

`OnChanging` is triggered prior to reallocation or destruction of the surface buffer.

---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandPassMouse
entity: TRubberbandPassMouse.ToLayerUnderCursor
kind: Property
scope: Public
declaration: "property ToLayerUnderCursor: Boolean read FLayerUnderCursor write FLayerUnderCursor default False;"
summary: "Controls whether mouse messages are passed to any underlying layer beneath the cursor."
---

## Description

When `ToLayerUnderCursor` is `True`, mouse events are forwarded to the topmost interactive layer located at the cursor position.

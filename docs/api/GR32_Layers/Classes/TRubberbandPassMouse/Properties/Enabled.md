---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandPassMouse
entity: TRubberbandPassMouse.Enabled
kind: Property
scope: Public
declaration: "property Enabled: Boolean read FEnabled write FEnabled default False;"
summary: "Enables or disables mouse event passthrough behavior."
---

## Description

When `Enabled` is `True`, mouse events are forwarded to target layers according to `ToChild` and `ToLayerUnderCursor`.

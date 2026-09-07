---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandPassMouse
entity: TRubberbandPassMouse.ToChild
kind: Property
scope: Public
declaration: "property ToChild: Boolean read FToChild write FToChild default False;"
summary: "Controls whether mouse messages are passed to the child layer."
---

## Description

When `ToChild` is `True`, mouse events received by the rubberband layer are forwarded to `ChildLayer`.

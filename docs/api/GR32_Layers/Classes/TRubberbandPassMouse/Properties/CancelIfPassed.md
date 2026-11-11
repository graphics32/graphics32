---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandPassMouse
entity: TRubberbandPassMouse.CancelIfPassed
kind: Property
scope: Public
declaration: "property CancelIfPassed: Boolean read FCancelIfPassed write FCancelIfPassed default False;"
summary: "Cancels further rubberband mouse handling if the mouse event was successfully passed to another layer."
---

## Description

When `CancelIfPassed` is `True`, rubberband drag operations do not initiate if a child or underlying layer handled the mouse event.

---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.BeginUpdate
kind: Method
declaration: "procedure BeginUpdate; virtual;"
summary: "Increments the update counter to defer change notifications."
---

## Description

Call `BeginUpdate` before making multiple modifications to the object state.

`BeginUpdate` increments `UpdateCount`. While `UpdateCount` is greater than zero, calls to [[Changed]] mark [[Modified]] as `True` instead of immediately firing the `OnChange` event.

:::warning Note
Every call to `BeginUpdate` must be paired with a corresponding call to the `EndUpdate` method. 
:::
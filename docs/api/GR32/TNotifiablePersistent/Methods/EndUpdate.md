---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.EndUpdate
kind: Method
declaration: "procedure EndUpdate; virtual;"
summary: "Decrements the update counter and triggers change notification if modified."
---

## Description

`EndUpdate` decrements `UpdateCount`. When `UpdateCount` reaches zero and `Modified` is `True`, `EndUpdate` calls `Changed` to trigger the `OnChange` event notification and resets `Modified` to `False`.

:::warning Note
Every call to `BeginUpdate` must be paired with a corresponding call to the `EndUpdate` method. 
:::

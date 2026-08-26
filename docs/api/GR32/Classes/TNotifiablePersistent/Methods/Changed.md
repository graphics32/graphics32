---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.Changed
kind: Method
declaration: "procedure Changed; virtual;"
summary: "Notifies listeners that the object state has changed."
---

## Description

`Changed` is called internally whenever the object state changes.

If state change notification has been *disabled* with `BeginLockUpdate` (thus `LockUpdateCount` > 0), then `Changed` does nothing.

If state change notification has been *suspended* with `BeginUpdate` (thus `UpdateCount` > 0), then `Changed` sets `Modified` to `True`, but defers the `OnChange` change notification.

Otherwise, `Changed` calls `DoChanged` to execute the `OnChange` event handler.
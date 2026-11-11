---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.DoChanged
kind: Method
scope: Protected
declaration: "procedure DoChanged; virtual;"
summary: "Executes the OnChange event handler."
---

## Description

`DoChanged` is a protected virtual method invoked by `Changed` when updates are not locked.

If assigned, `DoChanged` calls the `OnChange` event handler. Derived classes can override `DoChanged` to execute custom logic when the object state changes.
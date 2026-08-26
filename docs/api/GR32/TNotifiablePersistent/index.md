---
layout: doc
docType: api
unit: GR32
entity: TNotifiablePersistent
kind: Class
declaration: "TNotifiablePersistent = class(TPlainInterfacedPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
summary: "Persistent object subclass that manages change notifications (`OnChange` events) and change batching (`BeginUpdate` / `EndUpdate`)."
---

## Description

`TNotifiablePersistent` extends `TPlainInterfacedPersistent` by adding a deferred notification and update lock mechanism.

It allows callers to batch multiple state modifications between calls to `BeginUpdate` and `EndUpdate` (or `BeginLockUpdate` and `EndLockUpdate`), postponing (or disabling) `OnChange` events until the batch operation completes.

[members]

---
layout: doc
docType: api
unit: GR32
entity: TNotifiablePersistent
kind: Class
declaration: "TNotifiablePersistent = class(TPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
summary: "Persistent object subclass that manages change notifications (`OnChange` events) and change batching (`BeginUpdate` / `EndUpdate`)."
---

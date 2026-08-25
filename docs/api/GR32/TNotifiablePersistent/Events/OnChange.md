---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.OnChange
kind: Event
declaration: "property OnChange: TNotifyEvent read FOnChange write FOnChange;"
summary: "Fired when the object state changes."
---

## Description

The `OnChange` event is triggered when state changes occur and updates are not locked by `BeginUpdate` or `BeginLockUpdate`.

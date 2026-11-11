---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.LockUpdateCount
kind: Property
scope: Protected
declaration: "property LockUpdateCount: Integer read FLockUpdateCount;"
summary: "Returns the current nesting level of BeginLockUpdate calls."
---

## Description

The `LockUpdateCount` property indicates how many times `BeginLockUpdate` has been called without a matching `EndLockUpdate`.

While `LockUpdateCount > 0`, changes does not mark the object modified, and notifications are temporarily suspended.

---
layout: doc
docType: api
unit: GR32
parent: TThreadPersistent
entity: TThreadPersistent.LockCount
kind: Property
scope: Protected
declaration: "property LockCount: Integer read FLockCount;"
summary: "Returns the current nesting level of thread locks."
---

## Description

The `LockCount` property returns the current number of active `Lock` calls made by the owning thread.

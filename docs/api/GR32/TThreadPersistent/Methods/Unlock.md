---
layout: doc
docType: api
unit: GR32
parent: TThreadPersistent
entity: TThreadPersistent.Unlock
kind: Method
declaration: "procedure Unlock;"
summary: "Releases the internal critical section lock."
---

## Description

`Unlock` decrements `LockCount` and releases the critical section lock when `LockCount` reaches zero, allowing other waiting threads to acquire the lock.

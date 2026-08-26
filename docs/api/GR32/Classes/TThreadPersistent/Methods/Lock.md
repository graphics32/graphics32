---
layout: doc
docType: api
unit: GR32
parent: TThreadPersistent
entity: TThreadPersistent.Lock
kind: Method
declaration: "procedure Lock;"
summary: "Acquires the internal critical section lock."
---

## Description

`Lock` acquires the underlying critical section lock for the instance and increments `LockCount`. Calling `Lock` blocks other threads from accessing locked resources until `Unlock` is called.

Calls to `Lock` can be nested on the same thread, provided each call is paired with a corresponding `Unlock` call.

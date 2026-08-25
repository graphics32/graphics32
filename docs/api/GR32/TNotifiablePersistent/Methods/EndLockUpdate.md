---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.EndLockUpdate
kind: Method
declaration: "procedure EndLockUpdate;"
summary: "Resumes update state changes and change notifications."
---

## Description

`EndLockUpdate` decrements `LockUpdateCount` after a preceding call to `BeginLockUpdate`. When `LockUpdateCount` reaches zero, normal change notification processing resumes.

:::info
Calling `EndLockUpdate` does not cause a change notification to fire, even if `Modified` is `True`.
:::

:::warning Note
Every call to `BeginLockUpdate` must be paired with a preceding call to the `EndLockUpdate` method. 
:::
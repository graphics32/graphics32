---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.BeginLockUpdate
kind: Method
declaration: "procedure BeginLockUpdate;"
summary: "Supends update state changes and change notifications."
---

## Description

`BeginLockUpdate` increments `LockUpdateCount`. While `LockUpdateCount` is greater than zero, all [[Modified]] state changes and change notifications are suspended.

:::info
`BeginLockUpdate` does not alter the existing `Modified` state of the object. It only suspends `Modified` state changes until `EndLockUpdate` is called.
:::

:::warning Note
Every call to `BeginLockUpdate` must be paired with a corresponding call to the `EndLockUpdate` method. 
:::
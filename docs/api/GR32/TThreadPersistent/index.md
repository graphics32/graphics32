---
layout: doc
docType: api
unit: GR32
entity: TThreadPersistent
kind: Class
declaration: "TThreadPersistent = class(TNotifiablePersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
summary: "Ancestor for TBitmap32 and core graphics objects, adding thread-safe locking mechanisms in addition to change notifications."
---

## Description

`TThreadPersistent` extends `TNotifiablePersistent` by providing critical-section locking methods (`Lock` and `Unlock`). This ensures thread-safe access to graphics surfaces and data structures across concurrent threads.

:::warning
Inheriting from `TThreadPersistent` does not automatically make an object thread safe. `TThreadPersistent` merely provides a locking mechanism that you can employ to make your code thread safe.

Likewise, even though many Graphics32 classes inherit from `TThreadPersistent`, use of these classes are not inherently thread safe.
:::

## Constructors

| Name | Description |
| --- | --- |
| [Create](Constructors/Create.md) | Initializes a new instance of `TThreadPersistent` and creates its internal critical section. |

## Methods

| Name | Description |
| --- | --- |
| [Lock](Methods/Lock.md) | Acquires the internal critical section lock. |
| [Unlock](Methods/Unlock.md) | Releases the internal critical section lock. |

## Properties

| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [LockCount](Properties/LockCount.md) | `Integer` | Protected | Returns the current recursion level of critical section locks. |

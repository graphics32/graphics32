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

## Methods

| Name | Description |
| --- | --- |
| [BeginUpdate](Methods/BeginUpdate.md) | Increments the update counter to defer change notifications during batch modifications. |
| [EndUpdate](Methods/EndUpdate.md) | Decrements the update counter and triggers `Changed` if modifications occurred while suspended. |
| [BeginLockUpdate](Methods/BeginLockUpdate.md) | Increments the lock update counter. |
| [EndLockUpdate](Methods/EndLockUpdate.md) | Decrements the lock update counter. |
| [Changed](Methods/Changed.md) | Triggers the change notification flow or marks the object as modified if updates are suspended. |
| [DoChanged](Methods/DoChanged.md) | Protected virtual method that executes the `OnChange` event handler. |

## Properties

| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [UpdateCount](Properties/UpdateCount.md) | `Integer` | Protected | Current nest level of `BeginUpdate` / `EndUpdate` blocks. |
| [LockUpdateCount](Properties/LockUpdateCount.md) | `Integer` | Protected | Current nest level of `BeginLockUpdate` / `EndLockUpdate` blocks. |
| [Modified](Properties/Modified.md) | `Boolean` | Protected | Indicates whether changes occurred while `UpdateCount > 0`. |

## Events

| Name | Type | Description |
| --- | --- | --- |
| [OnChange](Events/OnChange.md) | `TNotifyEvent` | Fired when the object state changes and updates are not suspended. |

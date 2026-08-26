---
layout: doc
docType: api
unit: GR32
entity: TCustomBackend
kind: Class
declaration: "TCustomBackend = class(TThreadPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
summary: "Abstract base class for bitmap surface backends in Graphics32, managing raw pixel memory allocations and platform-specific surface handles."
---

## Description

`TCustomBackend` is the abstract base class for all backend memory and surface managers in Graphics32.

It abstracts the allocation, lifetime, and OS handles for 32-bit pixel buffers from `TCustomBitmap32`. Concrete backend subclasses (such as `TMemoryBackend`, `TGDIBackend`, or `TLCLBackend`) inherit from `TCustomBackend` and implement platform-specific surface allocation routines by overriding `InitializeSurface` and `FinalizeSurface`.

## Constructors

| Name | Description |
| --- | --- |
| [Create](Constructors/Create.md) | Initializes a new `TCustomBackend` instance, optionally attaching it to an owner bitmap. |

## Methods

| Name | Description |
| --- | --- |
| [Assign](Methods/Assign.md) | Copies pixel buffer memory and dimensions from another backend instance. |
| [ChangeSize](Methods/ChangeSize.md) | Resizes the backing surface and reallocates pixel memory buffer. |
| [Changing](Methods/Changing.md) | Protected virtual method that triggers the `OnChanging` event before surface modifications. |
| [Clear](Methods/Clear.md) | Deallocates the surface and resets dimensions to zero. |
| [Empty](Methods/Empty.md) | Returns `True` if the backend surface buffer is unallocated or empty. |
| [FinalizeSurface](Methods/FinalizeSurface.md) | Protected virtual method that frees allocated OS surface handles and memory buffers. |
| [InitializeSurface](Methods/InitializeSurface.md) | Protected virtual method that allocates memory buffers and OS handles for specified dimensions. |

## Properties

| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [Bits](Properties/Bits.md) | `PColor32Array` | Public | Pointer to the contiguous 32-bit ARGB pixel buffer memory array. |

## Events

| Name | Type | Description |
| --- | --- | --- |
| [OnChanging](Events/OnChanging.md) | `TNotifyEvent` | Fired immediately before backend surface dimensions or memory buffers change. |

---
layout: doc
docType: api
unit: GR32
entity: TCustomBackend
kind: Class
declaration: |
  type
    TCustomBackend = class(TThreadPersistent)
      ...
    TCustomBackendClass = class of TCustomBackend;
aliases: [TCustomBackendClass]
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

[members]

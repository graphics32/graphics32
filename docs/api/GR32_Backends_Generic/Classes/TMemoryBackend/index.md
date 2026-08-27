---
layout: doc
docType: api
unit: GR32_Backends_Generic
entity: TMemoryBackend
kind: Class
declaration: "TMemoryBackend = class(TCustomBackend)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TMemoryBackend
summary: "A lightweight surface backend that keeps 32-bit pixel buffers entirely in RAM memory."
---

# TMemoryBackend

`TMemoryBackend` is a cross-platform backend implementation that stores raw 32-bit pixel data directly in system heap memory without allocating operating system GUI device contexts or window handles.

## Description

`TMemoryBackend` inherits from `TCustomBackend` and overrides `InitializeSurface` and `FinalizeSurface` to manage memory buffers using standard `GetMem` and `FreeMem` calls.

It is ideal for headless image processing, server applications, off-screen rendering, or background threads where GDI/GUI window handles are unavailable or unnecessary.

[members]

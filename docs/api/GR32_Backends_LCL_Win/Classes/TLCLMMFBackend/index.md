---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
entity: TLCLMMFBackend
kind: Class
declaration: "TLCLMMFBackend = class(TLCLBackend)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TLCLBackend
  - TLCLMMFBackend
summary: "LCL surface backend backed by Windows memory-mapped files or swap space."
---

## Description

`TLCLMMFBackend` extends `TLCLBackend` to store DIB section buffers in memory-mapped disk files or Windows swap space.

`TLCLMMFBackend` overrides `PrepareFileMapping` to create file mapping objects, providing memory-mapped file storage while retaining LCL device context compatibility (`HDC`).

[members]

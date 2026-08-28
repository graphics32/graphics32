---
layout: doc
docType: api
unit: GR32_Backends_VCL
entity: TGDIMMFBackend
kind: Class
declaration: "TGDIMMFBackend = class(TGDIBackend)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TGDIBackend
  - TGDIMMFBackend
summary: "GDI surface backend backed by Windows memory-mapped files or swap space."
---

## Description

`TGDIMMFBackend` extends `TGDIBackend` to back DIB section buffers with memory-mapped files or system swap space.

`TGDIMMFBackend` overrides `PrepareFileMapping` to invoke file mapping creation via `TMMFBackend`, combining GDI device context support (`HDC`) with memory-mapped file storage.

[members]

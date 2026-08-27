---
layout: doc
docType: api
unit: GR32_Backends_Generic
entity: TMMFBackend
kind: Class
declaration: "TMMFBackend = class(TMemoryBackend)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TMemoryBackend
  - TMMFBackend
summary: "A surface backend that uses Windows Memory-Mapped Files (MMF) or system swap space for bitmap storage."
---

# TMMFBackend

`TMMFBackend` is a memory-mapped file surface backend designed for Windows applications that require handling ultra-large bitmap buffers exceeding physical RAM boundaries.

## Description

`TMMFBackend` allocates pixel buffers using Windows memory-mapped file views (`CreateFileMapping` / `MapViewOfFile`). Pixel buffers can be backed either by temporary disk files or directly by the operating system page file.

[members]

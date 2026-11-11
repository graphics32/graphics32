---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
entity: TLCLMemoryBackend
kind: Class
declaration: "TLCLMemoryBackend = class(TMemoryBackend, IPaintSupport, IDeviceContextSupport)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TMemoryBackend
  - TLCLMemoryBackend
summary: "Heap memory backend for Lazarus LCL rendering directly to TCanvas controls without allocating GDI handles."
---

## Description

`TLCLMemoryBackend` combines heap memory pixel allocations (`TMemoryBackend`) with Lazarus LCL repainting support.

`TLCLMemoryBackend` does not allocate GDI bitmap section handles or memory device contexts. When repainting onto an LCL `TCanvas`, it outputs pixel data directly using `SetDIBitsToDevice` on Windows LCL targets.

[members]

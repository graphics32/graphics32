---
layout: doc
docType: api
unit: GR32_Backends_VCL
entity: TGDIMemoryBackend
kind: Class
declaration: "TGDIMemoryBackend = class(TMemoryBackend, IPaintSupport, IDeviceContextSupport)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TMemoryBackend
  - TGDIMemoryBackend
summary: "A heap memory backend that renders directly to VCL TCanvas controls using SetDIBitsToDevice without allocating GDI handles."
---

# TGDIMemoryBackend

`TGDIMemoryBackend` combines pure heap memory allocation (`TMemoryBackend`) with VCL repainting support (`IPaintSupport` and `IDeviceContextSupport`).

## Description

Unlike `TGDIBackend`, `TGDIMemoryBackend` does not allocate GDI bitmap handles (`HBITMAP`) or memory device contexts (`HDC`). Instead, it uses `SetDIBitsToDevice` to blit raw heap pixel buffers directly onto destination `TCanvas` handles during paint operations.

[members]

---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
entity: TLCLBackend
kind: Class
declaration: "TLCLBackend = class(TCustomBackend, IPaintSupport, IBitmapContextSupport, IDeviceContextSupport, ITextSupport, IFontSupport, ITextToPathSupport, ICanvasSupport, IInteroperabilitySupport, IUpdateRectSupport)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TLCLBackend
summary: "Default surface backend for Graphics32 on Windows under Lazarus LCL, utilizing LCLIntf DIB sections and device contexts."
---

# TLCLBackend

`TLCLBackend` is the default backend implementation for Graphics32 on Windows under Lazarus LCL (`LCLWin32` / `LCLWin64`).

## Description

`TLCLBackend` manages 32-bit DIB surface buffers created via `LCLIntf.CreateDIBSection` and binds them to an LCL Memory Device Context (`HDC`).

It implements primary LCL surface interfaces:
- `IPaintSupport`: Repainting invalid areas onto LCL `TCanvas`.
- `IBitmapContextSupport`: Accessing `TBitmapInfo` headers and `HBITMAP` handles.
- `IDeviceContextSupport`: Interoperating with device context handles (`HDC`) and blitting.
- `ITextSupport` / `IFontSupport`: Native LCL font management and string rendering.
- `ITextToPathSupport`: Converting TrueType font glyph outlines into vector paths.
- `IInteroperabilitySupport`: Copying pixel content from LCL `TGraphic` objects into backend buffers.
- `ICanvasSupport`: LCL `TCanvas` wrapper integration.
- `IUpdateRectSupport`: LCL window control update region querying via `GetRandomRgn(..., SYSRGN)`.

[members]

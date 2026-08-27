---
layout: doc
docType: api
unit: GR32_Backends_VCL
entity: TGDIBackend
kind: Class
declaration: "TGDIBackend = class(TCustomBackend, IPaintSupport, IBitmapContextSupport, IDeviceContextSupport, ITextSupport, IFontSupport, ICanvasSupport, ITextToPathSupport, ITextToPathSupport2, IUpdateRectSupport)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomBackend
  - TGDIBackend
summary: "Default surface backend for Graphics32 on Windows, utilizing GDI DIB sections and device contexts."
---

# TGDIBackend

`TGDIBackend` is the default backend implementation for Graphics32 on Windows VCL platforms.

## Description

`TGDIBackend` manages 32-bit Device Independent Bitmaps (DIB sections) created via Windows `CreateDIBSection` and pairs them with a GDI Memory Device Context (`HDC`) created via `CreateCompatibleDC`.

It implements all primary surface interfaces:
- `IPaintSupport`: Repainting invalid areas onto `TCanvas`.
- `IBitmapContextSupport`: Accessing `TBitmapInfo` headers and `HBITMAP` handles.
- `IDeviceContextSupport`: Interoperating with GDI `HDC` handles and `BitBlt`/`StretchBlt`.
- `ITextSupport` / `IFontSupport`: Native GDI font management and string rendering.
- `ITextToPathSupport` / `ITextToPathSupport2`: Converting TrueType/OpenType font glyphs into vector paths using WinAPI GDI glyph outlines.
- `ICanvasSupport`: VCL `TCanvas` wrapper integration.
- `IUpdateRectSupport`: Windows control update region querying and invalidation.

[members]

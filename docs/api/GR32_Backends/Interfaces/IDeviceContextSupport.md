---
layout: doc
docType: api
unit: GR32_Backends
entity: IDeviceContextSupport
kind: Interface
declaration: "IDeviceContextSupport = interface(IUnknown)"
summary: "Interface for backends providing native OS device context handles (HDC) and bit-blitting operations."
---

# Interface IDeviceContextSupport

`IDeviceContextSupport` provides access to native OS GDI/LCL device context handles (`HDC`) and blitting/drawing routines.

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `Handle` | `HDC` | Read-only | Native OS GDI device context handle associated with the surface buffer. |

## Methods

### Draw
```pascal
procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
```
Blits/stretches pixel content from external device context `hSrc` onto the backend surface.

### DrawTo
```pascal
procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
```
Blits/stretches backend surface pixels onto an external target device context `hDst`.

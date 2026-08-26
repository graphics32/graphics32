---
layout: doc
docType: api
unit: GR32
entity: TBitmap32
kind: Class
declaration: "TBitmap32 = class(TCustomBitmap32)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TCustomBitmap32
  - TBitmap32
summary: "Primary 32-bit ARGB bitmap class in Graphics32, providing VCL/LCL Canvas, GDI device context (HDC), and OS surface handle bindings."
---

## Description

`TBitmap32` is the standard concrete 32-bit ARGB bitmap implementation in Graphics32. It inherits all core pixel buffer management, sub-pixel sampling, drawing primitives, blending modes, and resamplers from `TCustomBitmap32`.

In addition, `TBitmap32` integrates with host OS graphics frameworks (VCL on Windows, LCL on cross-platform GUI toolkits) by exposing a standard VCL/LCL `Canvas`, device context (`HDC`), GDI bitmap surface handle (`Handle`), `Font` property for text rendering, and GDI drawing routines (`TileTo`, HDC `Draw` / `DrawTo` overloads).

Ancestor Class: [[TCustomBitmap32]]

## Constructors & Destructors

| Name | Description |
| --- | --- |
| [Create](Constructors/Create.md) | Inherited constructor that instantiates a new `TBitmap32` bitmap instance. |

## Methods

| Name | Description |
| --- | --- |
| [Draw](Methods/Draw.md) | Draws a source bitmap, sub-rectangle, or GDI DC onto this bitmap. |
| [DrawTo](Methods/DrawTo.md) | Draws this bitmap onto a destination bitmap or GDI device context (HDC). |
| [GetPlatformBackendClass](Methods/GetPlatformBackendClass.md) | Returns the platform-default backend class used for surface management. |
| [TileTo](Methods/TileTo.md) | Tiles a source bitmap sub-rectangle repeatedly onto a target GDI device context (HDC). |

## Properties

| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [BitmapInfo](Properties/BitmapInfo.md) | `TBitmapInfo` | Public | Read-only GDI bitmap header information structure. |
| [Canvas](Properties/Canvas.md) | `TCanvas` | Public | VCL/LCL drawing canvas bound to the bitmap's device context (HDC). |
| [Font](Properties/Font.md) | `TFont` | Public | Font instance used for canvas text rendering operations. |
| [Handle](Properties/Handle.md) | `HBITMAP` | Public | OS GDI bitmap surface handle. |
| [HDC](Properties/HDC.md) | `HDC` | Public | OS GDI device context handle bound to pixel buffer surface. |

## Events

| Name | Type | Description |
| --- | --- | --- |
| [OnHandleChanged](Events/OnHandleChanged.md) | `TNotifyEvent` | Fired when the OS GDI bitmap surface handle (`Handle`) changes or is re-allocated. |

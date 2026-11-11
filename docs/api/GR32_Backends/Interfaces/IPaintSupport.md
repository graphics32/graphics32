---
layout: doc
docType: api
unit: GR32_Backends
entity: IPaintSupport
kind: Interface
declaration: "IPaintSupport = interface(IUnknown)"
summary: "Interface for backends handling control repainting and invalid rect transfer to TCanvas."
---

## Description

`IPaintSupport` handles repainting a `TBitmap32` buffer onto a target GUI control's `TCanvas`.

## Methods

### ImageNeeded
```pascal
procedure ImageNeeded;
```
Ensures that backend pixmap and surface resources are valid and allocated prior to rendering.

### CheckPixmap
```pascal
procedure CheckPixmap;
```
Verifies backend pixel format integrity and synchronization.

### DoPaint
```pascal
procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas); overload;
procedure DoPaint(ABuffer: TBitmap32; const AInvalidRect: TRect; ACanvas: TCanvas); overload;
```
Blits specified invalid rectangular region(s) of `ABuffer` onto the destination `ACanvas`.

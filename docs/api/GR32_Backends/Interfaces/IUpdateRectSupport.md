---
layout: doc
docType: api
unit: GR32_Backends
entity: IUpdateRectSupport
kind: Interface
declaration: "IUpdateRectSupport = interface(IUnknown)"
summary: "Interface for querying window update regions and invalidating control areas."
---

# Interface IUpdateRectSupport

`IUpdateRectSupport` provides control invalidation and update region retrieval routines for GUI controls (`TWinControl`).

## Methods

### InvalidateRect
```pascal
procedure InvalidateRect(AControl: TWinControl; const ARect: TRect);
```
Invalidates a specific rectangular region `ARect` of `AControl`, triggering a repaint.

### GetUpdateRects
```pascal
procedure GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer; var AFullUpdate: boolean);
```
Queries the operating system update region for `AControl` and populates `AUpdateRects` with individual bounding rectangles. Sets `AFullUpdate` to `True` if a full control repaint is required.

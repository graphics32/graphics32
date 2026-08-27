---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.GetUpdateRects
kind: Method
scope: Public
declaration: "procedure GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer; var AFullUpdate: boolean); overload;"
summary: "Retrieves invalid update regions from an LCL control handle."
parameters:
  - name: AControl
    type: TWinControl
    description: "Target control."
  - name: AUpdateRects
    type: TRectList
    description: "List receiving update rectangles."
  - name: AReservedCapacity
    type: integer
    description: "Capacity reserve."
  - name: AFullUpdate
    type: boolean
    description: "Set to True if full control repaint is required."
---

# TLCLBackend.GetUpdateRects

`GetUpdateRects` retrieves update regions using `GetRandomRgn(DC, UpdateRegion, SYSRGN)` to support Lazarus `WM_PAINT` handlers inside `BeginPaint`/`EndPaint`.

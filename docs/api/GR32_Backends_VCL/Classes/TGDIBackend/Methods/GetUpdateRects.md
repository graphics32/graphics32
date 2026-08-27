---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.GetUpdateRects
kind: Method
scope: Public
declaration: "procedure GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer; var AFullUpdate: boolean);"
summary: "Retrieves invalid update regions from a window control handle."
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

# TGDIBackend.GetUpdateRects

`GetUpdateRects` uses Windows GDI region APIs (`GetUpdateRgn` / `GetRegionData`) to extract invalid rectangles from `AControl`.

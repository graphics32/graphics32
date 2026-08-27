---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.InvalidateRect
kind: Method
scope: Public
declaration: "procedure InvalidateRect(AControl: TWinControl; const ARect: TRect);"
summary: "Invalidates a control region using WinAPI InvalidateRect."
parameters:
  - name: AControl
    type: TWinControl
    description: "Target control."
  - name: ARect
    type: TRect
    description: "Invalid rectangle."
---

# TGDIBackend.InvalidateRect

`InvalidateRect` invokes `WinAPI.Windows.InvalidateRect`.

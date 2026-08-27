---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.InvalidateRect
kind: Method
scope: Public
declaration: "procedure InvalidateRect(AControl: TWinControl; const ARect: TRect);"
summary: "Invalidates a control region using Windows.InvalidateRect."
parameters:
  - name: AControl
    type: TWinControl
    description: "Target control."
  - name: ARect
    type: TRect
    description: "Invalid rectangle."
---

# TLCLBackend.InvalidateRect

`InvalidateRect` invokes `Windows.InvalidateRect`.

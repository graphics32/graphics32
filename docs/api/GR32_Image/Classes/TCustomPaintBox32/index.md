---
layout: doc
docType: api
unit: GR32_Image
entity: TCustomPaintBox32
kind: Class
abstract: true
declaration: "TCustomPaintBox32 = class(TGraphics32ControlBaseClass);"
inheritance:
  - TGraphics32ControlBaseClass
  - TCustomPaintBox32
summary: "Abstract base class for 32-bit paint box controls with double-buffered TBitmap32 surface rendering."
seealso:
  - "[[TPaintBox32]]"
---

## Description

`TCustomPaintBox32` provides a base visual control for double-buffered rendering onto an internal 32-bit bitmap surface (`Buffer`). It handles buffer sizing, update and lock counts, repaint strategies ([[TRepaintMode]]), invalidation tracking, and GDI overlay events.

[members]

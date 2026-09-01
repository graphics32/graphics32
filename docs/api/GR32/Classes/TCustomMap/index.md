---
layout: doc
docType: api
unit: GR32
entity: TCustomMap
kind: Class
abstract: true
declaration: "TCustomMap = class(TThreadPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
summary: "Abstract base class for two-dimensional data containers (bitmaps, vector maps, ordinal maps) with dimensions and resizing capabilities."
---

## Description

`TCustomMap` is the abstract base class for all two-dimensional mapped data structures in Graphics32, including `TCustomBitmap32`, ordinal maps (`TByteMap`, `TFloatMap`), and vector maps (`TVectorMap`). It establishes standard `Width` and `Height` management, buffer resizing routines, and dimension change notifications via `OnResize`.

[members]

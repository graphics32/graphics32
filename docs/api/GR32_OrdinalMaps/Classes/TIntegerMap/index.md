---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TIntegerMap
kind: Class
declaration: "TIntegerMap = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TIntegerMap
summary: "Two-dimensional 32-bit signed integer map."
---

## Description

`TIntegerMap` is a 2D data map derived from [[TCustomMap]] storing 32-bit signed integer values (`Integer`).

It is used for storing discrete grid indices, spatial ID buffers, signed height fields, and coordinate maps. Elements can be accessed via `Value[X, Y]`, scanline pointers (`Scanline[Y]`), or element pointers (`ValPtr[X, Y]`).

[members]

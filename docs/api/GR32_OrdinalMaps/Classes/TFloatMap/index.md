---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TFloatMap
kind: Class
declaration: "TFloatMap = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TFloatMap
summary: "Two-dimensional floating-point (TFloat) map for high-precision analytical grid data."
---

## Description

`TFloatMap` is a 2D data map derived from [[TCustomMap]] storing single precision floating-point values (`TFloat`).

It is used for high-precision analytical calculations, distance fields, weight distributions, kernel buffers, and floating-point height fields. Elements can be accessed via `Value[X, Y]`, scanline pointers (`Scanline[Y]`), or element pointers (`ValPtr[X, Y]`).

[members]

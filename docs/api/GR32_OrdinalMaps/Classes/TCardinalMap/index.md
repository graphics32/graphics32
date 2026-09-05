---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TCardinalMap
kind: Class
declaration: "TCardinalMap = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TCardinalMap
summary: "Two-dimensional 32-bit unsigned cardinal integer map."
---

## Description

`TCardinalMap` is a 2D data map derived from [[TCustomMap]] storing 32-bit unsigned cardinal integer values (`Cardinal`).

It is used for storing 32-bit unsigned attributes, bit flags, packed color values, and memory addresses. Elements can be accessed via `Value[X, Y]`, scanline pointers (`Scanline[Y]`), or element pointers (`ValPtr[X, Y]`).

[members]

---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TWordMap
kind: Class
declaration: "TWordMap = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TWordMap
summary: "Two-dimensional 16-bit unsigned integer (Word) map."
---

## Description

`TWordMap` is a 2D data map derived from [[TCustomMap]] storing 16-bit unsigned integer values (`Word`).

It is commonly used for high-resolution 16-bit heightmaps, depth buffers, index maps, and 16-bit grayscale data arrays. Elements can be accessed via `Value[X, Y]`, scanline pointers (`Scanline[Y]`), or direct element pointers (`ValPtr[X, Y]`).

[members]

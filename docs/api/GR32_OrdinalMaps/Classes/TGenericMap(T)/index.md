---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TGenericMap<T>
kind: Class
declaration: "TGenericMap<T> = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TGenericMap<T>
summary: "Generic two-dimensional data map for storing arbitrary record or primitive value types."
---

## Description

`TGenericMap<T>` is a **generic** 2D data map derived from [[TCustomMap]] that manages a contiguous 2D grid of elements of arbitrary type `T`.

It provides dynamic buffer allocation and indexing (`Value[X, Y]`) for custom user record structures, non-ordinal types, and custom per-pixel attributes.

[members]

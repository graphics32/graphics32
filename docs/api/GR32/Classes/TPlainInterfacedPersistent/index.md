---
layout: doc
docType: api
unit: GR32
entity: TPlainInterfacedPersistent
kind: Class
abstract: true
declaration: "TPlainInterfacedPersistent = class(TPersistent, IInterface)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
summary: "Base class that provides lightweight IInterface implementation for TPersistent descendants with optional reference-counting."
---

## Description

`TPlainInterfacedPersistent` is a subclass of `TPersistent` that implements the `IInterface` interface. Unlike `TInterfacedObject`, instances of `TPlainInterfacedPersistent` are not reference-counted by default (`RefCounted = False`). This prevents unexpected destruction when casting to interface types or passing interface references.

Reference counting can be enabled per-instance by setting the protected `RefCounted` property to `True`.

[members]

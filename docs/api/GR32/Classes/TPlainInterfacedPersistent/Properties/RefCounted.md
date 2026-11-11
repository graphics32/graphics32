---
layout: doc
docType: api
unit: GR32
parent: TPlainInterfacedPersistent
entity: TPlainInterfacedPersistent.RefCounted
kind: Property
scope: Protected
declaration: "property RefCounted: Boolean read FRefCounted write FRefCounted;"
summary: "Determines whether lifetime is controlled by reference counting."
---

## Description

The `RefCounted` property controls whether interface reference management automatically destroys the object instance when its reference count drops to zero.

When set to `False` (the default), `_Release` returns the current reference count without destroying the instance. When set to `True`, lifetime behavior mimics standard `TInterfacedObject`, where releasing the last interface reference frees the instance.

---
layout: doc
docType: api
unit: GR32
parent: TPlainInterfacedPersistent
entity: TPlainInterfacedPersistent.RefCount
kind: Property
declaration: "property RefCount: Integer read FRefCount;"
summary: "Retrieves the current reference count for the instance."
---

## Description

The `RefCount` property returns the current number of active *interface* references held for this instance.

When `RefCounted` is `False` (the default), interface `_AddRef` and `_Release` calls do not trigger automatic object destruction even if `RefCount` drops to zero.

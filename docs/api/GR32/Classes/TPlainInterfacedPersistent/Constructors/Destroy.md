---
layout: doc
docType: api
unit: GR32
entity: TPlainInterfacedPersistent.Destroy
kind: Destructor
declaration: "destructor Destroy; override;"
summary: "Disposes of the object instance, releasing any allocated resources."
---

::: warning
Do not call `Destroy` directly. Use `Free` to safely check for `nil` before destroying the instance.
:::

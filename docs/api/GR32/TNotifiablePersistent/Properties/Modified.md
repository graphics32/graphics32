---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.Modified
kind: Property
scope: Protected
declaration: "property Modified: Boolean read FModified;"
summary: "Indicates whether modifications occurred while updates were suspended."
---

## Description

The `Modified` property is set to `True` if `Changed` is called while `UpdateCount > 0` *and* `LockUpdateCount = 0`.

When `EndUpdate` reduces `UpdateCount` to zero, `Modified` is checked to determine if the `OnChange` event should fire.

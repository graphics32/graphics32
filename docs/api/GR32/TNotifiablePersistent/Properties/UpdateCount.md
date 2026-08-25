---
layout: doc
docType: api
unit: GR32
parent: TNotifiablePersistent
entity: TNotifiablePersistent.UpdateCount
kind: Property
scope: Protected
declaration: "property UpdateCount: Integer read FUpdateCount;"
summary: "Returns the current nesting level of BeginUpdate calls."
---

## Description

The `UpdateCount` property indicates how many times `BeginUpdate` has been called without a matching `EndUpdate`. While `UpdateCount > 0`, notifications are deferred until `UpdateCount` reaches zero.

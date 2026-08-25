---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.SetWidth
kind: Method
scope: Protected
declaration: "procedure SetWidth(NewWidth: Integer); virtual;"
summary: "Protected write method for the Width property."
parameters:
  - name: NewWidth
    type: Integer
    description: "Requested width."
---

## Description

`SetWidth` calls `SetSize` to update `Width` while preserving current `Height`.

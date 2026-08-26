---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.SetHeight
kind: Method
scope: Protected
declaration: "procedure SetHeight(NewHeight: Integer); virtual;"
summary: "Protected write method for the Height property."
parameters:
  - name: NewHeight
    type: Integer
    description: "Requested height."
---

## Description

`SetHeight` calls `SetSize` to update `Height` while preserving current `Width`.

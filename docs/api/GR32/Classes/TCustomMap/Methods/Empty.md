---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.Empty
kind: Method
declaration: "function Empty: Boolean; virtual;"
summary: "Determines whether the map has zero area."
returns:
  - type: Boolean
    description: "Returns `True` if map dimensions are zero or buffer is unallocated; otherwise `False`."
---

## Description

`Empty` returns `True` if either `Width <= 0` or `Height <= 0`. Returns `False` when both dimensions are positive.

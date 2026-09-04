---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.Empty
kind: Method
declaration: "function Empty: Boolean; override;"
summary: "Returns True if the vector map width or height is zero, or if the vector buffer is unallocated."
returns:
  - type: Boolean
    description: "Returns `True` if the vector map has zero area or an unallocated buffer; otherwise `False`."
---

## Description

`Empty` returns `True` if `Width = 0`, `Height = 0`, or the internal vector buffer pointer is `nil`.

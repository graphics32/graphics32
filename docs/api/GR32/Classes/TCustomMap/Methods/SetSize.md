---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.SetSize
kind: Method
declaration: "function SetSize(NewWidth, NewHeight: Integer; ClearBuffer: Boolean = True): Boolean; virtual;"
summary: "Resizes the map to new dimensions."
parameters:
  - name: NewWidth
    type: Integer
    description: "New horizontal dimension."
  - name: NewHeight
    type: Integer
    description: "New vertical dimension."
  - name: ClearBuffer
    type: Boolean
    description: "When True (default), clears buffer contents on resize."
returns:
  - type: Boolean
    description: "Returns `True` if dimensions were modified or buffer reallocated; otherwise `False`."
---

## Description

`SetSize` changes the dimensions of the map to `NewWidth` and `NewHeight`. If `ClearBuffer` is `True`, buffer contents are cleared or reinitialized. Returns `True` if the map dimensions changed.

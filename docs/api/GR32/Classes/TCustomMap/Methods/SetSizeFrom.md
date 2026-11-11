---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.SetSizeFrom
kind: Method
declaration: "function SetSizeFrom(Source: TPersistent; ClearBuffer: Boolean = True): Boolean;"
summary: "Copies dimensions from another object."
parameters:
  - name: Source
    type: TPersistent
    description: "Source object (typically a TCustomMap instance) to copy dimensions from."
  - name: ClearBuffer
    type: Boolean
    description: "When True (default), clears buffer contents on resize."
returns:
  - type: Boolean
    description: "Returns `True` if map dimensions were updated to match the source object; otherwise `False`."
---

## Description

`SetSizeFrom` extracts dimensions from `Source` and calls `SetSize` to resize this instance to match. Returns `True` if dimensions were changed.

 `Source` can be an object based on one of the following classes:
 - `TCustomMap`
 - `TGraphic`
 - `TControl`

If `Source = nil` then the size is set to `(0, 0)`.
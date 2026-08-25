---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.ChangeSize
kind: Method
scope: Protected
declaration: "procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean = True); virtual;"
summary: "Protected virtual method performing low-level buffer reallocation and dimension updates."
parameters:
  - name: Width
    type: Integer
    description: "Reference to current width variable."
  - name: Height
    type: Integer
    description: "Reference to current height variable."
  - name: NewWidth
    type: Integer
    description: "Requested new width."
  - name: NewHeight
    type: Integer
    description: "Requested new height."
  - name: ClearBuffer
    type: Boolean
    description: "When True, buffer content should be cleared."
---

## Description

`ChangeSize` is an internal protected method overridden by derived classes (`TCustomBitmap32`, `TByteMap`, etc.) to reallocate memory buffers and update `Width` and `Height`.

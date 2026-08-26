---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.ChangeSize
kind: Method
scope: Public
declaration: "procedure ChangeSize(out Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean = True); virtual;"
summary: "Resizes the surface buffer to specified dimensions."
parameters:
  - name: Width, Height
    type: Integer
    description: "Output parameters updated with the new width and height."
  - name: NewWidth, NewHeight
    type: Integer
    description: "Target surface dimensions."
  - name: ClearBuffer
    type: Boolean
    description: "If True (default), clears new surface buffer to zero."
---

## Description

`ChangeSize` invokes `Changing`, calls `FinalizeSurface` to release previous allocations, and calls `InitializeSurface` to allocate the new dimensions `(NewWidth, NewHeight)`.

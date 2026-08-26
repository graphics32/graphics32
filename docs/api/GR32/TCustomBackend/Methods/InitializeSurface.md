---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.InitializeSurface
kind: Method
scope: Protected
declaration: "procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); virtual;"
summary: "Protected virtual method overridden by concrete backends to allocate surface memory and OS handles."
parameters:
  - name: NewWidth, NewHeight
    type: Integer
    description: "Dimensions of the new surface to allocate."
  - name: ClearBuffer
    type: Boolean
    description: "If True, initializes allocated pixel memory to zero."
---

## Description

`InitializeSurface` is implemented by backend subclasses (e.g. `TMemoryBackend`, `TGDIBackend`) to allocate raw memory buffers or OS surface handles.

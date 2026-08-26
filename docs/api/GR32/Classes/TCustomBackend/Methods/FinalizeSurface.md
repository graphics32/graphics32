---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.FinalizeSurface
kind: Method
scope: Protected
declaration: "procedure FinalizeSurface; virtual;"
summary: "Protected virtual method overridden by concrete backends to deallocate surface memory and OS handles."
---

## Description

`FinalizeSurface` is implemented by backend subclasses to release allocated memory buffers, device contexts, or bitmap handles.

---
layout: doc
docType: api
unit: GR32_Backends
entity: RestoreBackend
kind: Function
declaration: "procedure RestoreBackend(TargetBitmap: TCustomBitmap32; const SavedBackend: TCustomBackend);"
summary: "Restores a previously saved surface backend onto a target bitmap."
parameters:
  - name: TargetBitmap
    type: TCustomBitmap32
    description: "Bitmap target receiving the restored backend."
  - name: SavedBackend
    type: TCustomBackend
    description: "Backend instance previously returned by RequireBackendSupport."
---

## Description

`RestoreBackend` assigns `SavedBackend` back to `TargetBitmap.Backend` if `SavedBackend` is non-nil, reversing a temporary backend switch.

---
layout: doc
docType: api
unit: GR32_Backends
entity: RequireBackendSupport
kind: Function
declaration: "procedure RequireBackendSupport(TargetBitmap: TCustomBitmap32; RequiredInterfaces: array of TGUID; Mode: TRequireOperatorMode; UseOptimizedDestructiveSwitchMethod: Boolean; out ReleasedBackend: TCustomBackend);"
summary: "Verifies if a bitmap's active backend supports required interface contracts, switching to a compatible backend if necessary."
parameters:
  - name: TargetBitmap
    type: TCustomBitmap32
    description: "Bitmap target whose active backend is inspected."
  - name: RequiredInterfaces
    type: array of TGUID
    description: "Array of interface GUIDs required by the caller."
  - name: Mode
    type: TRequireOperatorMode
    description: "Logical mode (romAnd / romOr) for interface compatibility matching."
  - name: UseOptimizedDestructiveSwitchMethod
    type: Boolean
    description: "If True, clears bitmap dimensions during backend switch to avoid unnecessary buffer copying."
  - name: ReleasedBackend
    type: TCustomBackend
    description: "Outputs the previous backend instance if replaced, or nil if no switch was performed."
---

# Routine RequireBackendSupport

`RequireBackendSupport` inspects `TargetBitmap.Backend` to check if it supports the interface GUIDs in `RequiredInterfaces`. If unsupported, it releases the existing backend and instantiates a compatible platform backend.

## Example

```pascal
var
  SavedBackend: TCustomBackend;
begin
  RequireBackendSupport(Bitmap, [IDeviceContextSupport], romAnd, True, SavedBackend);
  try
    // Perform DC operations
  finally
    RestoreBackend(Bitmap, SavedBackend);
  end;
end;
```

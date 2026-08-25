---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.GetPlatformBackendClass
kind: Method
scope: Protected
declaration: "class function GetPlatformBackendClass: TCustomBackendClass; virtual;"
summary: "Returns the backend class used to instantiate the default backend interface."
---

`GetPlatformBackendClass` is a protected class method that returns the `TCustomBackend` derived class to use when the [[Create | constructor]] doesn't explicitly specify one.

By default `GetPlatformBackendClass` returns [[TMemoryBackend]], but derived classes can can override `GetPlatformBackendClass` to return something else.
For example, [[TBitmap32]], overrides `GetPlatformBackendClass` to return a suitable *platform specific* backend class, such as `TGDIBackend` on Delphi/Windows or `TLCLBackend` on LCL/FPC.

## See also
- [[TBitmap32.GetPlatformBackendClass]]

---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.Changed
kind: Method
scope: Public
declaration: "procedure Changed; override;"
summary: "Notifies the backend of surface modification and updates internal canvas handles."
---

# TGDIBackend.Changed

`Changed` is invoked whenever the backend surface data changes, ensuring that the internal `FCanvas` instance remains bound to the active `HDC` handle before calling `inherited Changed`.

---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.PrepareFileMapping
kind: Method
scope: Protected
declaration: "procedure PrepareFileMapping(NewWidth, NewHeight: Integer); virtual;"
summary: "Virtual method overridden by memory-mapped backend subclasses to prepare file mapping objects."
parameters:
  - name: NewWidth, NewHeight
    type: Integer
    description: "New surface width and height."
---

# TGDIBackend.PrepareFileMapping

`PrepareFileMapping` is a virtual extension point called during surface initialization prior to creating the DIB section. Subclasses such as `TGDIMMFBackend` override this method to construct file mapping handles.

---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.PrepareFileMapping
kind: Method
scope: Protected
declaration: "procedure PrepareFileMapping(NewWidth, NewHeight: Integer); virtual;"
summary: "Virtual method overridden by memory-mapped backend subclasses."
parameters:
  - name: NewWidth, NewHeight
    type: Integer
    description: "Surface dimensions."
---

# TLCLBackend.PrepareFileMapping

`PrepareFileMapping` is a virtual hook called during DIB allocation.

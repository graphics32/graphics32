---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.CopyFrom
kind: Method
scope: Public
declaration: "function CopyFrom(Graphic: TGraphic): Boolean; overload;"
summary: "Copies image contents from an LCL TGraphic object."
parameters:
  - name: Graphic
    type: TGraphic
    description: "Source graphic."
---

# TLCLBackend.CopyFrom

`CopyFrom` draws `Graphic` onto `Canvas`, transferring image contents to the backend buffer.

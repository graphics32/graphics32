---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.Assign
kind: Method
scope: Public
declaration: "procedure Assign(Source: TPersistent); override;"
summary: "Copies pixel buffer memory and dimensions from another backend instance."
parameters:
  - name: Source
    type: TPersistent
    description: "Source backend instance to copy from."
---

## Description

`Assign` copies buffer dimensions and performs a raw memory copy from `Source` into this backend.

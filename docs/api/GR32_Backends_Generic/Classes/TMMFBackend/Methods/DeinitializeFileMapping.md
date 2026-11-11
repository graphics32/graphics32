---
layout: doc
docType: api
unit: GR32_Backends_Generic
parent: TMMFBackend
entity: TMMFBackend.DeinitializeFileMapping
kind: Method
scope: Public
declaration: "class procedure DeinitializeFileMapping(MapHandle, MapFileHandle: THandle; const MapFileName: string);"
summary: "Class method closing mapping handles and deleting temporary backing files."
parameters:
  - name: MapHandle
    type: THandle
    description: "Windows file mapping handle to close."
  - name: MapFileHandle
    type: THandle
    description: "Windows file handle to close."
  - name: MapFileName
    type: string
    description: "Path of temporary file to delete if existing."
---

## Description

`DeinitializeFileMapping` closes open file mapping handles and file handles, deleting temporary disk files created during surface allocation.

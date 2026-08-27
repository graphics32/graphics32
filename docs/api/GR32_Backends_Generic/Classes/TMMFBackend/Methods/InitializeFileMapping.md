---
layout: doc
docType: api
unit: GR32_Backends_Generic
parent: TMMFBackend
entity: TMMFBackend.InitializeFileMapping
kind: Method
scope: Public
declaration: "class procedure InitializeFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string);"
summary: "Class method initializing file mapping handles and creating target directory structures if required."
parameters:
  - name: MapHandle
    type: THandle
    description: "Output file mapping handle initialized to 0."
  - name: MapFileHandle
    type: THandle
    description: "Output OS file handle initialized to INVALID_HANDLE_VALUE."
  - name: MapFileName
    type: string
    description: "File path for mapping; directory path is created if missing."
---

# TMMFBackend.InitializeFileMapping

`InitializeFileMapping` prepares handle variables prior to creating file mappings and ensures destination folder paths exist.

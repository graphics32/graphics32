---
layout: doc
docType: api
unit: GR32_Backends_Generic
parent: TMMFBackend
entity: TMMFBackend.CreateFileMapping
kind: Method
scope: Public
declaration: "class procedure CreateFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string; IsTemporary: Boolean; NewWidth, NewHeight: Integer);"
summary: "Class method creating Windows file handles and file mapping objects for specified surface dimensions."
parameters:
  - name: MapHandle
    type: THandle
    description: "Receives created file mapping handle."
  - name: MapFileHandle
    type: THandle
    description: "Receives created OS file handle."
  - name: MapFileName
    type: string
    description: "Path to map file."
  - name: IsTemporary
    type: Boolean
    description: "Whether file is temporary."
  - name: NewWidth, NewHeight
    type: Integer
    description: "Dimensions used to calculate required buffer byte size."
---

## Description

`CreateFileMapping` opens or creates the disk file (or system page file allocation) and invokes `Windows.CreateFileMapping` to allocate memory for `NewWidth * NewHeight * 4` bytes.

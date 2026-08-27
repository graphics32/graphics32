---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLMMFBackend
entity: TLCLMMFBackend.Create
kind: Constructor
declaration: "constructor Create(Owner: TBitmap32; IsTemporary: Boolean = True; const MapFileName: string = ''); virtual;"
summary: "Initializes a new TLCLMMFBackend instance."
parameters:
  - name: Owner
    type: TBitmap32
    description: "Owner bitmap."
  - name: IsTemporary
    type: Boolean
    description: "Whether the mapped file is temporary."
  - name: MapFileName
    type: string
    description: "Optional disk file path for mapping."
---

# TLCLMMFBackend.Create

`Create` initializes an LCL memory-mapped backend.

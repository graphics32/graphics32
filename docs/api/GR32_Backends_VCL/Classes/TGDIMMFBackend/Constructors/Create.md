---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIMMFBackend
entity: TGDIMMFBackend.Create
kind: Constructor
declaration: "constructor Create(Owner: TBitmap32; IsTemporary: Boolean = True; const MapFileName: string = ''); virtual;"
summary: "Initializes a new TGDIMMFBackend instance bound to an Owner bitmap."
parameters:
  - name: Owner
    type: TBitmap32
    description: "Owner bitmap instance."
  - name: IsTemporary
    type: Boolean
    description: "Whether the mapping file is temporary."
  - name: MapFileName
    type: string
    description: "Optional custom disk file path for mapping."
---

# TGDIMMFBackend.Create

`Create` initializes a GDI memory-mapped file backend.

---
layout: doc
docType: api
unit: GR32_Backends_Generic
parent: TMMFBackend
entity: TMMFBackend.Create
kind: Constructor
declaration: "constructor Create(Owner: TCustomBitmap32; IsTemporary: Boolean = True; const MapFileName: string = ''); virtual;"
summary: "Initializes a new TMMFBackend instance bound to a TCustomBitmap32 owner."
parameters:
  - name: Owner
    type: TCustomBitmap32
    description: "Bitmap instance owning this backend."
  - name: IsTemporary
    type: Boolean
    description: "If True, creates a temporary file that is automatically deleted when closed."
  - name: MapFileName
    type: string
    description: "Optional path to an external disk file used for mapping. If empty, system swap file or auto-generated temp file is used."
---

## Description

`Create` initializes a memory-mapped file backend. If `MapFileName` is provided, the backend maps pixel data to that specific file on disk. If `IsTemporary` is `True` and `MapFileName` is empty, a temporary disk file is generated in the system temporary directory.

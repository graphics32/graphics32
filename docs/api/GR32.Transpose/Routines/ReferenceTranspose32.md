---
layout: doc
docType: api
unit: GR32.Transpose
entity: ReferenceTranspose32
kind: Function
declaration: "procedure ReferenceTranspose32(Src, Dst: Pointer; Width, Height: Integer);"
summary: "Reference Pascal implementation of 32-bit pixel matrix transposition."
parameters:
  - name: Src
    type: Pointer
    description: "Pointer to source 32-bit pixel buffer."
  - name: Dst
    type: Pointer
    description: "Pointer to destination 32-bit pixel buffer."
  - name: Width
    type: Integer
    description: "Source width in pixels."
  - name: Height
    type: Integer
    description: "Source height in pixels."
seealso:
  - Transpose32
---

## Description

`ReferenceTranspose32` provides a plain, unoptimized Pascal reference implementation of 32-bit matrix transposition. It loops through source pixels row by row and copies them to transposed destination positions.

This procedure is primarily used as a correctness baseline for validating SIMD and cache-optimized transpose implementations in unit tests.

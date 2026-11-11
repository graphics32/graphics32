---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.Vectors
kind: Property
declaration: "property Vectors: PFixedPointArray read GetVectors;"
summary: "Provides direct pointer access to the internal fixed-point displacement vector buffer."
---

## Description

`Vectors` returns a pointer (`PFixedPointArray`) to the first element in the internal contiguous 16.16 fixed-point vector memory buffer (`FVectors`).

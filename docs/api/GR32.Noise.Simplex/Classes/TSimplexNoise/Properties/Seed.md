---
layout: doc
docType: api
unit: GR32.Noise.Simplex
parent: TSimplexNoise
entity: TSimplexNoise.Seed
kind: Property
scope: Public
declaration: "property Seed: Int64 read FSeed;"
summary: "Returns the 64-bit integer seed value used to initialize the instance's permutation lookup tables."
---

## Description

`Seed` returns the 64-bit integer seed value passed during object construction (or automatically generated if the parameterless constructor was used).

The seed initializes the internal 512-byte permutation table and modulo-12 lookup table. Two distinct `TSimplexNoise` instances with identical `Seed` values and identical `SeedMult` values will produce identical noise values at any given coordinate.

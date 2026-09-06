---
layout: doc
docType: api
unit: GR32.Noise.Simplex
parent: TSimplexNoise
entity: TSimplexNoise.SeedMult
kind: Property
scope: Public
declaration: "class property SeedMult: Int64 read FSeedMultiplier write FSeedMultiplier;"
summary: "Specifies the global 64-bit seed multiplier used during pseudo-random permutation table generation."
---

## Description

`SeedMult` is a class property containing the 64-bit linear congruential seed multiplier.

During constructor initialization, `SeedMult` is multiplied with the initial seed value across sequential permutation array indices to scramble bytes into the 512-entry internal permutation lookup table.

By default, `SeedMult` is initialized in the class constructor to `85123154182917`. Modifying `SeedMult` globally alters the permutation scrambling algorithm for all newly created `TSimplexNoise` instances.

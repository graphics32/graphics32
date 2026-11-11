---
layout: doc
docType: api
unit: GR32.Noise.Simplex
parent: TSimplexNoise
entity: TSimplexNoise.Create
kind: Constructor
summary: "Initializes a new instance of TSimplexNoise with either an automatic system time seed or a custom seed."
overloads:
  - signature: "constructor Create; overload;"
    summary: "Creates a new TSimplexNoise instance using an automatically generated seed derived from system performance counter and system time."
  - signature: "constructor Create(const ASeed: Int64); overload;"
    summary: "Creates a new TSimplexNoise instance with a specified 64-bit integer seed for deterministic noise generation."
    parameters:
      - name: ASeed
        type: Int64
        description: "Custom 64-bit seed value used to populate the internal permutation lookup tables."
---

## Description

The `Create` constructor initializes a `TSimplexNoise` instance and populates its internal 512-entry permutation table and modulo-12 lookup table.

- Calling parameterless `Create` retrieves an pseudo-random initial seed based on system performance ticks and system time, ensuring varied noise output across different runs.
- Calling `Create(ASeed)` sets a explicit seed value. Passing the same `ASeed` integer value across different instances or program executions generates identical permutation lookup tables, guaranteeing deterministic and reproducible noise patterns.

## Example

```pascal
var
  DeterministicNoise: TSimplexNoise;
  RandomNoise: TSimplexNoise;
begin
  // Create instance with fixed seed (reproducible pattern)
  DeterministicNoise := TSimplexNoise.Create(42);

  // Create instance with automatic pseudo-random seed
  RandomNoise := TSimplexNoise.Create;
  try
    // Use noise instances...
  finally
    DeterministicNoise.Free;
    RandomNoise.Free;
  end;
end;
```

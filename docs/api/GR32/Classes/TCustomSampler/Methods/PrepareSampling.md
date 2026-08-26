---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.PrepareSampling
kind: Method
scope: Public
declaration: "procedure PrepareSampling; virtual;"
summary: "Prepares internal state, caches, and nested samplers before a sampling sequence begins."
---

## Description

`PrepareSampling` initializes a sampler for an upcoming sampling sequence.

Callers (such as rasterizers, polygon fillers, or resampler loops) should invoke `PrepareSampling` once before calling `GetSampleInt`, `GetSampleFixed`, or `GetSampleFloat` in a loop. Derived samplers override `PrepareSampling` to cache expensive calculations, pre-compute transformation matrices, allocate temporary buffers, or propagate the `PrepareSampling` call to nested child samplers.

Every call to `PrepareSampling` must be paired with a corresponding call to `FinalizeSampling` when sampling is complete.

## Example

```pascal
var
  X, Y: Integer;
begin
  Sampler.PrepareSampling;
  try
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        DestBits[Y * Width + X] := Sampler.GetSampleInt(X, Y);
  finally
    Sampler.FinalizeSampling;
  end;
end;
```

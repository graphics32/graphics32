---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.FinalizeSampling
kind: Method
scope: Public
declaration: "procedure FinalizeSampling; virtual;"
summary: "Finalizes sampling state and cleans up temporary resources allocated during PrepareSampling."
---

## Description

`FinalizeSampling` cleans up internal state and resources allocated during `PrepareSampling`.

Callers should invoke `FinalizeSampling` immediately after completing a sampling loop. Derived samplers override `FinalizeSampling` to release temporary memory buffers, reset state flags, or propagate the call to nested child samplers.

## Example

```pascal
Sampler.PrepareSampling;
try
  // Perform sampling operations
  Color := Sampler.GetSampleFloat(10.5, 20.5);
finally
  Sampler.FinalizeSampling;
end;
```

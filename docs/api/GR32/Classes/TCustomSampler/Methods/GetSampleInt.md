---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.GetSampleInt
kind: Method
scope: Public
declaration: "function GetSampleInt(X, Y: Integer): TColor32; virtual;"
summary: "Evaluates and returns a TColor32 sample at integer pixel coordinates (X, Y)."
parameters:
  - name: X
    type: Integer
    description: "Horizontal coordinate of the sample position."
  - name: Y
    type: Integer
    description: "Vertical coordinate of the sample position."
returns:
  - type: TColor32
    description: "The sampled 32-bit ARGB `TColor32` color at integer coordinate `(X, Y)`."
---

## Description

`GetSampleInt` retrieves a 32-bit ARGB color sample at integer pixel coordinates $(X, Y)$.

In the base `TCustomSampler` class, `GetSampleInt` converts $(X, Y)$ into 16.16 fixed-point values and delegates computation to `GetSampleFixed`. Derived sampler classes may override `GetSampleInt` directly to provide optimized integer-coordinate evaluations.

## Example

```pascal
var
  SampleColor: TColor32;
begin
  Sampler.PrepareSampling;
  try
    SampleColor := Sampler.GetSampleInt(100, 50);
  finally
    Sampler.FinalizeSampling;
  end;
end;
```

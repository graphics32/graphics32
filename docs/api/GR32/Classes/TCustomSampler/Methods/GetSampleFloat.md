---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.GetSampleFloat
kind: Method
scope: Public
declaration: "function GetSampleFloat(X, Y: TFloat): TColor32; virtual;"
summary: "Evaluates and returns a TColor32 sample at single-precision floating-point coordinates (X, Y)."
parameters:
  - name: X
    type: TFloat
    description: "Horizontal sub-pixel coordinate of the sample position."
  - name: Y
    type: TFloat
    description: "Vertical sub-pixel coordinate of the sample position."
returns:
  - type: TColor32
    description: "The sampled 32-bit ARGB `TColor32` color at floating-point coordinate `(X, Y)`."
---

## Description

`GetSampleFloat` retrieves a 32-bit ARGB color sample at sub-pixel coordinates $(X, Y)$ specified using single-precision floating-point (`TFloat`).

This is the primary sampling entry point for continuous mathematical functions, gradients, and sub-pixel image interpolation routines. In `TCustomSampler`, `GetSampleFloat` converts $(X, Y)$ to fixed-point format and delegates to `GetSampleFixed`. Custom float-based samplers override `GetSampleFloat` to evaluate colors continuously across continuous domain coordinates.

## Example

```pascal
var
  SampleColor: TColor32;
begin
  Sampler.PrepareSampling;
  try
    SampleColor := Sampler.GetSampleFloat(12.75, 45.3);
  finally
    Sampler.FinalizeSampling;
  end;
end;
```

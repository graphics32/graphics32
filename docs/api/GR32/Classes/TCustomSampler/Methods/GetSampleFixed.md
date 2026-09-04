---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.GetSampleFixed
kind: Method
scope: Public
declaration: "function GetSampleFixed(X, Y: TFixed): TColor32; virtual;"
summary: "Evaluates and returns a TColor32 sample at 16.16 fixed-point coordinates (X, Y)."
parameters:
  - name: X
    type: TFixed
    description: "Horizontal coordinate of the sample position in 16.16 fixed-point format."
  - name: Y
    type: TFixed
    description: "Vertical coordinate of the sample position in 16.16 fixed-point format."
returns:
  - type: TColor32
    description: "The sampled 32-bit ARGB `TColor32` color at fixed-point coordinate `(X, Y)`."
---

## Description

`GetSampleFixed` retrieves a 32-bit ARGB color sample at sub-pixel coordinates $(X, Y)$ specified using 16.16 fixed-point format (`TFixed`).

In the base `TCustomSampler` class, `GetSampleFixed` converts $(X, Y)$ to single-precision floating-point values and delegates computation to `GetSampleFloat`. Descendants optimized for fixed-point arithmetic (such as fixed-point resamplers or pattern generators) override this method to perform fast integer/fixed-point sampling.

## Example

```pascal
var
  FX, FY: TFixed;
  SampleColor: TColor32;
begin
  FX := Fixed(10.5);
  FY := Fixed(20.25);

  Sampler.PrepareSampling;
  try
    SampleColor := Sampler.GetSampleFixed(FX, FY);
  finally
    Sampler.FinalizeSampling;
  end;
end;
```

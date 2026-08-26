---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.GetSampleBounds
kind: Method
scope: Public
declaration: "function GetSampleBounds: TFloatRect; virtual;"
summary: "Returns the spatial boundary rectangle (TFloatRect) within which valid samples exist."
---

## Description

`GetSampleBounds` returns a floating-point rectangle (`TFloatRect`) representing the spatial boundary limits of the sampler.

In the base `TCustomSampler` class, `GetSampleBounds` returns an infinite rectangle (`-Infinity, -Infinity, Infinity, Infinity`). Derived classes (such as `TCustomResampler`) override this method to return the actual coordinate bounds of the underlying source bitmap or clipping rectangle.

## Example

```pascal
var
  Bounds: TFloatRect;
begin
  if Sampler.HasBounds then
  begin
    Bounds := Sampler.GetSampleBounds;
    // Process within specified spatial bounds
  end;
end;
```

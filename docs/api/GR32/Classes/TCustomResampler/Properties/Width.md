---
layout: doc
docType: api
unit: GR32
parent: TCustomResampler
entity: TCustomResampler.Width
kind: Property
scope: Public
declaration: "property Width: TFloat read GetWidth;"
summary: "Retrieves the effective kernel sampling radius or width for kernel-based resamplers."
---

## Description

`Width` returns the effective sampling width (or kernel radius) of the resampler as a floating-point value (`TFloat`).

For simple point or linear resamplers without extended kernels, `Width` returns `0.0`. Derived kernel resamplers (such as `TKernelResampler`) override `GetWidth` to report their active filter kernel width.

## Example

```pascal
var
  W: TFloat;
begin
  W := Resampler.Width;
end;
```

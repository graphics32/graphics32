---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex.DefuzzAtZero
kind: Field
scope: Public
summary: "Class variable enabling automatic zero-snapping for near-zero real or imaginary parts during TComplex instance construction."
declaration: "class var DefuzzAtZero: Boolean;"
---

## Description

`DefuzzAtZero` determines whether constructor factory functions (`From`, `FromPolar`) automatically invoke `Defuzz` to snap near-zero floating-point values back to exact zero (`0.0`).

Due to IEEE 754 floating-point inaccuracies, evaluations like $\cos(\pi / 2)$ or $e^{i \pi} + 1$ produce residual values near $10^{-16}$. When `DefuzzAtZero` is `True` (default), `Math.IsZero` checks are applied to both `Real` and `Imaginary` parts upon construction, resetting components below the zero threshold to `0.0`.

Set `DefuzzAtZero` to `False` if exact raw floating-point output without zero-thresholding is required.

## Example

```pascal
var
  C: TComplex;
begin
  TComplex.DefuzzAtZero := True; // Default
  C := TComplex.From(1.0e-18, 5.0);
  // C.Real is automatically defuzzed to 0.0
  WriteLn('Real: ', C.Real); // 0.0
end;
```

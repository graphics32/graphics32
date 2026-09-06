---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex.Real
kind: Field
scope: Public
summary: "The real double-precision scalar component of the complex number."
declaration: "Real: Double;"
seealso:
  - "[[TComplex.Imaginary]]"
---

## Description

`Real` stores the real double-precision floating-point component ($a$) of the complex number $z = a + bi$.

## Example

```pascal
var
  C: TComplex;
begin
  C := TComplex.From(3.0, 4.0);
  // C.Real is 3.0
  WriteLn('Real part: ', C.Real);
end;
```

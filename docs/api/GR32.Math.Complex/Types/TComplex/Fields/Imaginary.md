---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex.Imaginary
kind: Field
scope: Public
summary: "The imaginary double-precision scalar component of the complex number."
declaration: "Imaginary: Double;"
seealso:
  - "[[TComplex.Real]]"
---

## Description

`Imaginary` stores the imaginary double-precision floating-point component ($b$) of the complex number $z = a + bi$.

## Example

```pascal
var
  C: TComplex;
begin
  C := TComplex.From(3.0, 4.0);
  // C.Imaginary is 4.0
  WriteLn('Imaginary part: ', C.Imaginary);
end;
```

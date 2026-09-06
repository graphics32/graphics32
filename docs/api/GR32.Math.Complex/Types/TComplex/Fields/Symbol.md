---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex.Symbol
kind: Field
scope: Public
summary: "Class variable defining the string token used for the imaginary unit during string formatting and parsing."
declaration: "class var Symbol: string;"
seealso:
  - "[[TComplex.SymbolBeforeImaginary]]"
  - "[[TComplex.ToString]]"
  - "[[TComplex.Parse]]"
---

## Description

`Symbol` specifies the text representation of the imaginary unit $i$ used by `ToString` and `Parse`. By default, `Symbol` is initialized to `'i'`.

In engineering applications (such as electrical engineering) where $j$ is conventionally used for the imaginary unit to avoid confusion with electric current $i$, `Symbol` can be set to `'j'`.

## Example

```pascal
var
  C: TComplex;
begin
  TComplex.Symbol := 'j';
  C := TComplex.From(2.0, 5.0);
  // Outputs "2 + 5j"
  WriteLn(C.ToString);
  TComplex.Symbol := 'i'; // Restore default
end;
```

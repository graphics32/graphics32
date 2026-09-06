---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex.SymbolBeforeImaginary
kind: Field
scope: Public
summary: "Class variable controlling the relative placement of the imaginary unit symbol during string output formatting."
declaration: "class var SymbolBeforeImaginary: Boolean;"
seealso:
  - "[[TComplex.Symbol]]"
  - "[[TComplex.ToString]]"
  - "[[TComplex.Parse]]"
---

## Description

`SymbolBeforeImaginary` controls whether the imaginary symbol (such as `'i'` or `'j'`) is rendered before or after the numerical imaginary magnitude when calling `ToString`.

- **`False` (Default)**: Formats the symbol after the numeric value (e.g. `3 + 4i`).
- **`True`**: Formats the symbol before the numeric value (e.g. `3 + i4`).

## Example

```pascal
var
  C: TComplex;
begin
  C := TComplex.From(3.0, 4.0);

  TComplex.SymbolBeforeImaginary := False;
  WriteLn(C.ToString); // "3 + 4i"

  TComplex.SymbolBeforeImaginary := True;
  WriteLn(C.ToString); // "3 + i4"

  TComplex.SymbolBeforeImaginary := False; // Restore default
end;
```

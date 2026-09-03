---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.Clear
kind: Method
declaration: "procedure Clear; virtual;"
summary: "Resets internal path state and resets control point origins."
---

## Description

`Clear` resets internal path tracking state in `TCustomPath`. In derived classes like [[TFlattenedPath]], calling `Clear` also purges all stored vertex arrays and closed path flags.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(10, 10);
    Canvas.LineTo(100, 10);
    Canvas.EndPath;

    // Reset the path for new construction
    Canvas.Clear;
  finally
    Canvas.Free;
  end;
end;
```

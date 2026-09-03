---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.Rectangle
kind: Method
declaration: "procedure Rectangle(const Rect: TFloatRect); virtual;"
summary: "Appends a closed rectangular path to the vector path."
parameters:
  - name: Rect
    type: TFloatRect
    description: "Rectangle bounds."
---

## Description

`Rectangle` converts the boundary coordinates in `Rect` into a closed four-vertex polygon path and appends it to the path builder.

Calling `Rectangle` automatically starts a new closed sub-path.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.Rectangle(FloatRect(10.0, 10.0, 150.0, 100.0));
  finally
    Canvas.Free;
  end;
end;
```

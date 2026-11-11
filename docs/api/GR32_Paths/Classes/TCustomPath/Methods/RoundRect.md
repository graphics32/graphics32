---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.RoundRect
kind: Method
declaration: "procedure RoundRect(const Rect: TFloatRect; const Radius: TFloat); virtual;"
summary: "Appends a closed rounded rectangle path with rounded corners."
parameters:
  - name: Rect
    type: TFloatRect
    description: "Rectangle boundary coordinates."
  - name: Radius
    type: TFloat
    description: "Corner rounding radius in pixels."
---

## Description

`RoundRect` constructs a closed rectangular path with rounded corners of specified `Radius` and appends it to the path builder.

Calling `RoundRect` automatically starts a new closed sub-path.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.RoundRect(FloatRect(20.0, 20.0, 180.0, 120.0), 15.0);
  finally
    Canvas.Free;
  end;
end;
```

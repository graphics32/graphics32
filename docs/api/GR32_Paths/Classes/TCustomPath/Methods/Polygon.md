---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.Polygon
kind: Method
declaration: "procedure Polygon(const APoints: TArrayOfFloatPoint); virtual;"
summary: "Appends a closed polygon path defined by an array of vertices."
parameters:
  - name: APoints
    type: TArrayOfFloatPoint
    description: "Array of float points defining polygon vertices."
---

## Description

`Polygon` appends a closed polygon path defined by vertex array `APoints`.

Calling `Polygon` implicitly ends any open path segment, moves to `APoints[0]`, appends line segments connecting each subsequent point, and automatically closes the path back to `APoints[0]`.

## Example

```pascal
var
  Canvas: TCanvas32;
  Pts: TArrayOfFloatPoint;
begin
  SetLength(Pts, 4);
  Pts[0] := FloatPoint(100.0, 20.0);
  Pts[1] := FloatPoint(180.0, 100.0);
  Pts[2] := FloatPoint(100.0, 180.0);
  Pts[3] := FloatPoint(20.0, 100.0);

  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.Polygon(Pts);
  finally
    Canvas.Free;
  end;
end;
```

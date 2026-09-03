---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.PolyPolyLine
kind: Method
declaration: "procedure PolyPolyLine(const APoints: TArrayOfArrayOfFloatPoint); virtual;"
summary: "Appends multiple disconnected polylines from a 2D vertex array."
parameters:
  - name: APoints
    type: TArrayOfArrayOfFloatPoint
    description: "2D array of point arrays, where each sub-array represents a distinct polyline segment."
---

## Description

`PolyPolyLine` iterates through all polyline vertex arrays in `APoints`, calling [[PolyLine]] for each sub-array and ending each sub-path between polylines.

## Example

```pascal
var
  Canvas: TCanvas32;
  Lines: TArrayOfArrayOfFloatPoint;
begin
  SetLength(Lines, 2);

  // Line 1
  SetLength(Lines[0], 2);
  Lines[0][0] := FloatPoint(10.0, 10.0);
  Lines[0][1] := FloatPoint(100.0, 10.0);

  // Line 2
  SetLength(Lines[1], 2);
  Lines[1][0] := FloatPoint(10.0, 30.0);
  Lines[1][1] := FloatPoint(100.0, 30.0);

  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.PolyPolyLine(Lines);
  finally
    Canvas.Free;
  end;
end;
```

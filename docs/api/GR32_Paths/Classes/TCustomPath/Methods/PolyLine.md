---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.PolyLine
kind: Method
declaration: "procedure PolyLine(const APoints: TArrayOfFloatPoint; AOffset: integer = 0); virtual;"
summary: "Appends a connected sequence of line segments from a vertex array."
parameters:
  - name: APoints
    type: TArrayOfFloatPoint
    description: "Array of float points defining polyline vertices."
  - name: AOffset
    type: Integer
    description: "Index in APoints array from which to start appending points (default is 0)."
---

## Description

`PolyLine` appends connected straight line segments for all vertices in `APoints` starting from index `AOffset`.

If `AOffset` is `0`, `PolyLine` iterates through all elements of `APoints`, adding each vertex via [[LineTo]].

## Example

```pascal
var
  Canvas: TCanvas32;
  Pts: TArrayOfFloatPoint;
begin
  SetLength(Pts, 3);
  Pts[0] := FloatPoint(10.0, 10.0);
  Pts[1] := FloatPoint(100.0, 50.0);
  Pts[2] := FloatPoint(190.0, 10.0);

  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(Pts[0]);
    Canvas.PolyLine(Pts, 1);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```

---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.PolyPolygon
kind: Method
declaration: "procedure PolyPolygon(const APoints: TArrayOfArrayOfFloatPoint); virtual;"
summary: "Appends multiple closed polygon contours defined by a 2D vertex array."
parameters:
  - name: APoints
    type: TArrayOfArrayOfFloatPoint
    description: "2D array of point arrays, where each sub-array represents a distinct closed polygon contour."
---

## Description

`PolyPolygon` iterates through all polygon vertex arrays in `APoints`, calling [[Polygon]] for each sub-array to append multiple closed polygon contours.

## Example

```pascal
var
  Canvas: TCanvas32;
  Polys: TArrayOfArrayOfFloatPoint;
begin
  SetLength(Polys, 2);

  // Outer polygon
  SetLength(Polys[0], 4);
  Polys[0][0] := FloatPoint(10.0, 10.0);
  Polys[0][1] := FloatPoint(190.0, 10.0);
  Polys[0][2] := FloatPoint(190.0, 190.0);
  Polys[0][3] := FloatPoint(10.0, 190.0);

  // Inner hole polygon
  SetLength(Polys[1], 4);
  Polys[1][0] := FloatPoint(50.0, 50.0);
  Polys[1][1] := FloatPoint(150.0, 50.0);
  Polys[1][2] := FloatPoint(150.0, 150.0);
  Polys[1][3] := FloatPoint(50.0, 150.0);

  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.PolyPolygon(Polys);
  finally
    Canvas.Free;
  end;
end;
```

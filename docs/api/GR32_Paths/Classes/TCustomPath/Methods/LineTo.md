---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.LineTo
kind: Method
summary: "Appends a straight line segment from the current position to a target coordinate."
overloads:
  - signature: "procedure LineTo(const X, Y: TFloat); overload;"
    summary: "Appends a line segment to absolute coordinate (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Destination X and Y coordinates."

  - signature: "procedure LineTo(const P: TFloatPoint); overload; virtual;"
    summary: "Appends a line segment to absolute point P."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Destination point."
---

## Description

`LineTo` appends a straight line segment connecting [[CurrentPoint]] to a specified absolute destination point and updates `CurrentPoint` to the target location.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(10.0, 10.0);
    Canvas.LineTo(200.0, 10.0);
    Canvas.LineTo(FloatPoint(200.0, 150.0));
    Canvas.EndPath(False);
  finally
    Canvas.Free;
  end;
end;
```

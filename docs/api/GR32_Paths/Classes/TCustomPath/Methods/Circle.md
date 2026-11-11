---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.Circle
kind: Method
summary: "Appends a circular closed path to the vector path."
overloads:
  - signature: "procedure Circle(const Cx, Cy, Radius: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;"
    summary: "Appends a circle centered at (Cx, Cy) with specified radius."
    parameters:
      - name: Cx, Cy
        type: TFloat
        description: "Center X and Y coordinates."
      - name: Radius
        type: TFloat
        description: "Circle radius."
      - name: Steps
        type: Integer
        description: "Number of linear segments used to approximate the circle (default is DefaultCircleSteps)."

  - signature: "procedure Circle(const Center: TFloatPoint; Radius: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;"
    summary: "Appends a circle centered at Center point with specified radius."
    parameters:
      - name: Center
        type: TFloatPoint
        description: "Center point."
      - name: Radius
        type: TFloat
        description: "Circle radius."
      - name: Steps
        type: Integer
        description: "Number of linear segments used to approximate the circle."
---

## Description

`Circle` constructs a circular closed polygon path centered at `(Cx, Cy)` or `Center` with specified `Radius` and appends it to the path builder.

- `Steps` controls the number of polygonal segments generated around the circular perimeter (defaulting to [[DefaultCircleSteps]]).

Calling `Circle` automatically starts a new closed sub-path.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Draw a circle centered at (150, 150) with radius 50
    Canvas.Circle(150.0, 150.0, 50.0);
  finally
    Canvas.Free;
  end;
end;
```

---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.Ellipse
kind: Method
summary: "Appends an elliptical closed path to the vector path."
overloads:
  - signature: "procedure Ellipse(Rx, Ry: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;"
    summary: "Appends an ellipse centered at CurrentPoint with radii Rx and Ry."
    parameters:
      - name: Rx
        type: TFloat
        description: "Horizontal radius."
      - name: Ry
        type: TFloat
        description: "Vertical radius."
      - name: Steps
        type: Integer
        description: "Number of linear segments used to approximate the ellipse (default is DefaultCircleSteps)."

  - signature: "procedure Ellipse(const Cx, Cy, Rx, Ry: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;"
    summary: "Appends an ellipse centered at (Cx, Cy) with radii Rx and Ry."
    parameters:
      - name: Cx, Cy
        type: TFloat
        description: "Center X and Y coordinates."
      - name: Rx, Ry
        type: TFloat
        description: "Horizontal and vertical radii."
      - name: Steps
        type: Integer
        description: "Number of linear segments used to approximate the ellipse."
---

## Description

`Ellipse` constructs an elliptical closed polygon path with horizontal radius `Rx` and vertical radius `Ry` and appends it to the path builder.

- If center coordinates `(Cx, Cy)` are omitted, the ellipse is centered at [[CurrentPoint]].
- `Steps` controls the number of polygonal segments generated around the ellipse perimeter (defaulting to [[DefaultCircleSteps]]).

Calling `Ellipse` automatically starts a new closed sub-path.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Draw an ellipse centered at (100, 100) with Rx=80, Ry=40
    Canvas.Ellipse(100.0, 100.0, 80.0, 40.0);
  finally
    Canvas.Free;
  end;
end;
```

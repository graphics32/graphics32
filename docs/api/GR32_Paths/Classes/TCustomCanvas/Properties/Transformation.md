---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomCanvas
entity: TCustomCanvas.Transformation
kind: Property
declaration: "property Transformation: TTransformation read FTransformation write SetTransformation;"
summary: "Specifies a coordinate transformation applied to path points prior to rendering."
---

## Description

`Transformation` references a [[TTransformation]] object (such as [[TAffineTransformation]]) that transforms flattened path coordinates during canvas drawing operations.

Assigning a new transformation triggers a canvas change notification, causing active paths to be re-drawn with the applied spatial transformation.

## Example

```pascal
var
  Canvas: TCanvas32;
  Affine: TAffineTransformation;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  Affine := TAffineTransformation.Create;
  try
    // Apply a 45-degree rotation transformation
    Affine.Rotate(100.0, 100.0, 45.0);
    Canvas.Transformation := Affine;

    // Draw rectangle; coordinates will be rotated by 45 degrees
    Canvas.Rectangle(FloatRect(50.0, 50.0, 150.0, 150.0));
  finally
    Affine.Free;
    Canvas.Free;
  end;
end;
```

---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.Renderer
kind: Property
declaration: "property Renderer: TPolygonRenderer32 read FRenderer write SetRenderer;"
summary: "Specifies the software polygon renderer instance used to rasterize vector path shapes onto the target bitmap."
---

## Description

`Renderer` specifies the [[TPolygonRenderer32]] instance used to rasterize flattened vector path contours onto the target [[Bitmap]].

Assigning a new renderer frees the existing renderer instance and updates the canvas drawing pipeline.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Assign a VPR polygon renderer
    Canvas.Renderer := TPolygonRenderer32VPR.Create;

    Canvas.Circle(100.0, 100.0, 40.0);
  finally
    Canvas.Free;
  end;
end;
```

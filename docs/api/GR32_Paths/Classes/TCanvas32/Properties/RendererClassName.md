---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.RendererClassName
kind: Property
declaration: "property RendererClassName: string read GetRendererClassName write SetRendererClassName;"
summary: "Specifies the class name of the active polygon renderer used by the canvas."
---

## Description

`RendererClassName` gets or sets the class name of the polygon renderer instance ([[Renderer]]) attached to `TCanvas32`.

Setting `RendererClassName` searches the registered polygon renderer registry (`PolygonRendererList`) and instantiates a new renderer matching the specified class name (for example `'TPolygonRenderer32VPR'`).

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Switch renderer class by registered name
    Canvas.RendererClassName := 'TPolygonRenderer32VPR';

    Canvas.Rectangle(FloatRect(10.0, 10.0, 100.0, 100.0));
  finally
    Canvas.Free;
  end;
end;
```

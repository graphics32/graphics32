---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.Create
kind: Constructor
declaration: "constructor Create(ABitmap: TBitmap32); reintroduce; virtual;"
summary: "Creates a new TCanvas32 instance bound to a target TBitmap32."
parameters:
  - name: ABitmap
    type: TBitmap32
    description: "Target bitmap surface on which vector path shapes and text will be rendered. Must not be nil."
---

## Description

`Create` instantiates a new `TCanvas32` vector canvas bound to target bitmap `ABitmap`.

During initialization, `Create`:
- Assigns [[Bitmap]] reference (`ABitmap`).
- Instantiates a default polygon renderer instance ([[Renderer]]) via `GetPolygonRendererClass`.
- Initializes the brush collection ([[Brushes]]) with a change listener attached to trigger canvas redraws on brush modifications.

Raises an exception if `ABitmap` is `nil`.

## Example

```pascal
var
  Bitmap: TBitmap32;
  Canvas: TCanvas32;
  SolidBrush: TSolidBrush;
begin
  Bitmap := TBitmap32.Create(400, 300);
  try
    Bitmap.Clear(clWhite32);

    Canvas := TCanvas32.Create(Bitmap);
    try
      SolidBrush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
      SolidBrush.FillColor := clRed32;

      Canvas.Rectangle(FloatRect(50.0, 50.0, 250.0, 200.0));
    finally
      Canvas.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;
```

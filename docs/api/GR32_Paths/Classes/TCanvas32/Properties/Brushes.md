---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.Brushes
kind: Property
declaration: "property Brushes: TBrushCollection read FBrushes;"
summary: "Read-only collection of vector stroke and fill brushes applied during path rendering."
---

## Description

`Brushes` returns the [[TBrushCollection]] instance managed by `TCanvas32`.

When rendering paths, `TCanvas32` iterates through visible brushes in `Brushes` and executes their rendering pass (`PolyPolygonFS` or `PolyPolygonMixedFS`) on the polygon renderer.

Brushes added to `Brushes` can include:
- [[TSolidBrush]]: Solid color fill.
- [[TStrokeBrush]]: Vector outline stroke with configurable line width, join styles, and end cap styles.
- [[TDashedBrush]]: Dashed vector stroke patterns.
- [[TGrowBrush]]: Polygon expansion/dilation fills.
- [[TNestedBrush]]: Composite brush groupings.

## Example

```pascal
var
  Canvas: TCanvas32;
  FillBrush: TSolidBrush;
  StrokeBrush: TStrokeBrush;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Add solid fill brush
    FillBrush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
    FillBrush.FillColor := clBlue32;

    // Add stroke outline brush
    StrokeBrush := TStrokeBrush(Canvas.Brushes.Add(TStrokeBrush));
    StrokeBrush.FillColor := clBlack32;
    StrokeBrush.StrokeWidth := 2.0;

    // Draw shape with both fill and stroke
    Canvas.RoundRect(FloatRect(20.0, 20.0, 180.0, 120.0), 10.0);
  finally
    Canvas.Free;
  end;
end;

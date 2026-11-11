---
layout: doc
docType: api
unit: GR32_Paths
entity: TCanvas32
kind: Class
declaration: "TCanvas32 = class(TCustomCanvas)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomPath
  - TFlattenedPath
  - TCustomCanvas
  - TCanvas32
summary: "High-level 2D vector canvas rendering engine bound to a TBitmap32 target, managing brushes, text rendering, and polygon rasterization."
---

## Description

`TCanvas32` is the primary 2D vector drawing canvas in Graphics32. It binds path construction directives inherited from [[TCustomPath]] and [[TFlattenedPath]] directly to a target [[TBitmap32]] surface.

Key capabilities of `TCanvas32` include:
- **Bitmap Drawing Target**: Operates directly on an assigned [[Bitmap]] instance ([[TBitmap32]]).
- **Brush Collection Pipeline**: Manages a collection of vector stroke and fill brushes in [[Brushes]] ([[TBrushCollection]]), applying solid colors, strokes, dashes, or custom pattern fillers during path rendering.
- **Configurable Polygon Renderer**: Uses an anti-aliased software polygon renderer ([[Renderer]], [[RendererClassName]]) such as `TPolygonRenderer32VPR` or `TPolygonRenderer32LCD` to rasterize paths.
- **Vector Text Outline Rendering**: Converts TrueType/OpenType font outlines into vector path geometry via [[RenderText]] and measures text layout bounds via [[MeasureText]].

[members]

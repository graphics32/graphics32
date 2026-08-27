---
layout: doc
docType: api
unit: GR32_Backends
entity: ITextToPathSupport2
kind: Interface
declaration: "ITextToPathSupport2 = interface(ITextToPathSupport)"
summary: "Extended interface for text-to-path conversion with advanced TTextLayout configuration."
---

# Interface ITextToPathSupport2

`ITextToPathSupport2` extends `ITextToPathSupport` by accepting a structured `TTextLayout` object for advanced text alignment, line wrapping, and multi-line formatting.

## Methods

### TextToPath
```pascal
procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: string; const Layout: TTextLayout); overload;
procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: string; const Layout: TTextLayout); overload;
```
Appends character glyph outline contours of `Text` to `Path` using the layout options specified in `Layout`.

### MeasureText
```pascal
function MeasureText(const DstRect: TFloatRect; const Text: string; const Layout: TTextLayout): TFloatRect; overload;
```
Measures the floating-point bounding box of `Text` using the specified `Layout` settings.

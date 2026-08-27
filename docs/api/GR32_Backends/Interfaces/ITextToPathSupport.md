---
layout: doc
docType: api
unit: GR32_Backends
entity: ITextToPathSupport
kind: Interface
declaration: "ITextToPathSupport = interface(IUnknown)"
summary: "Interface for converting text string character glyphs into vector path contours."
---

# Interface ITextToPathSupport

`ITextToPathSupport` allows converting text glyph outlines into Graphics32 vector paths (`TCustomPath`) and measuring text bounding boxes in floating-point coordinates.

## Methods

### TextToPath
```pascal
procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: string; Flags: Cardinal = 0); overload;
procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: string; Flags: Cardinal = 0); overload;
```
Appends character glyph outline contours of `Text` to the target `Path` at position (`X`, `Y`) or within `DstRect`.

### MeasureText
```pascal
function MeasureText(const DstRect: TFloatRect; const Text: string; Flags: Cardinal = 0): TFloatRect;
```
Measures the floating-point bounding rectangle required to display `Text` given alignment flags and layout constraints.

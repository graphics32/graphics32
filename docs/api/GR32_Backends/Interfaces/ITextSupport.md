---
layout: doc
docType: api
unit: GR32_Backends
entity: ITextSupport
kind: Interface
declaration: "ITextSupport = interface(IUnknown)"
summary: "Interface for surface backends capable of rendering string text and calculating text extents."
---

# Interface ITextSupport

`ITextSupport` defines the contract for backends that support native string text rendering (`Textout`) and text measurement (`TextExtent`).

## Methods

### Textout
```pascal
procedure Textout(X, Y: Integer; const Text: String); overload;
procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: String); overload;
procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: String); overload;
```
Renders a text string at the designated coordinates or within a bounding/clipping rectangle using the backend's current font settings.

### TextExtent
```pascal
function TextExtent(const Text: String): TSize;
```
Calculates and returns the width and height (extent in pixels) of the specified text string when rendered with the backend's active font.

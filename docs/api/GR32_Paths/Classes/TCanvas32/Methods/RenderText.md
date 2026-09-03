---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.RenderText
kind: Method
summary: "Converts text outlines into vector path geometry and appends them to the canvas path."
overloads:
  - signature: "procedure RenderText(X, Y: TFloat; const Text: string); overload;"
    summary: "Renders text at position (X, Y) using default text layout settings."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Text baseline origin coordinates."
      - name: Text
        type: string
        description: "Text string to render."

  - signature: "procedure RenderText(X, Y: TFloat; const Text: string; Flags: Cardinal); overload;"
    summary: "Renders text at position (X, Y) using text alignment flags."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Text baseline origin coordinates."
      - name: Text
        type: string
        description: "Text string to render."
      - name: Flags
        type: Cardinal
        description: "Win32 DrawText-compatible formatting flags (e.g., DT_CENTER, DT_VCENTER)."

  - signature: "procedure RenderText(X, Y: TFloat; const Text: string; const Layout: TTextLayout); overload;"
    summary: "Renders text at position (X, Y) using a TTextLayout structure."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Text baseline origin coordinates."
      - name: Text
        type: string
        description: "Text string to render."
      - name: Layout
        type: TTextLayout
        description: "Text layout configuration."

  - signature: "procedure RenderText(const DstRect: TFloatRect; const Text: string; Flags: Cardinal); overload;"
    summary: "Renders text formatted within destination rectangle DstRect using text formatting flags."
    parameters:
      - name: DstRect
        type: TFloatRect
        description: "Destination rectangle for text layout and alignment."
      - name: Text
        type: string
        description: "Text string to render."
      - name: Flags
        type: Cardinal
        description: "Formatting flags controlling text alignment and line wrapping."

  - signature: "procedure RenderText(const DstRect: TFloatRect; const Text: string; const Layout: TTextLayout); overload;"
    summary: "Renders text formatted within destination rectangle DstRect using a TTextLayout structure."
    parameters:
      - name: DstRect
        type: TFloatRect
        description: "Destination rectangle for text layout and alignment."
      - name: Text
        type: string
        description: "Text string to render."
      - name: Layout
        type: TTextLayout
        description: "Text layout configuration."
---

## Description

`RenderText` converts character font outlines into vector path contours and appends them to the active canvas path.

Font outlines are converted using backend text-to-path interfaces (`ITextToPathSupport` or `ITextToPathSupport2`) supported by the target [[Bitmap]] backend.

The converted glyph contours are drawn using the current canvas brushes and polygon renderer.

## Example

```pascal
var
  Canvas: TCanvas32;
  SolidBrush: TSolidBrush;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    SolidBrush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
    SolidBrush.FillColor := clBlack32;

    // Render text string at coordinate (20, 40)
    Canvas.RenderText(20.0, 40.0, 'Hello Graphics32!');
  finally
    Canvas.Free;
  end;
end;
```

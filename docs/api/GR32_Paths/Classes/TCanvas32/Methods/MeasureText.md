---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.MeasureText
kind: Method
summary: "Measures the bounding rectangle required to render specified text within a destination rectangle."
overloads:
  - signature: "function MeasureText(const DstRect: TFloatRect; const Text: string; Flags: Cardinal): TFloatRect; overload;"
    summary: "Measures text bounding rectangle using formatting flags."
    parameters:
      - name: DstRect
        type: TFloatRect
        description: "Target destination rectangle constraint."
      - name: Text
        type: string
        description: "Text string to measure."
      - name: Flags
        type: Cardinal
        description: "Formatting flags controlling text alignment and wrapping."

  - signature: "function MeasureText(const DstRect: TFloatRect; const Text: string; const Layout: TTextLayout): TFloatRect; overload;"
    summary: "Measures text bounding rectangle using a TTextLayout structure."
    parameters:
      - name: DstRect
        type: TFloatRect
        description: "Target destination rectangle constraint."
      - name: Text
        type: string
        description: "Text string to measure."
      - name: Layout
        type: TTextLayout
        description: "Text layout configuration."
---

## Description

`MeasureText` computes the tight bounding rectangle (`TFloatRect`) required to layout and render `Text` within `DstRect` according to the specified formatting flags or `TTextLayout` options.

## Example

```pascal
var
  Canvas: TCanvas32;
  ConstraintRect, MeasuredRect: TFloatRect;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    ConstraintRect := FloatRect(0.0, 0.0, 200.0, 1000.0);
    MeasuredRect := Canvas.MeasureText(ConstraintRect, 'Sample multiline text to measure');

    WriteLn(Format('Measured text height: %.1f', [MeasuredRect.Bottom - MeasuredRect.Top]));
  finally
    Canvas.Free;
  end;
end;
```

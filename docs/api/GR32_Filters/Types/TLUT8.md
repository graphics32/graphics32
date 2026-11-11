---
layout: doc
docType: api
unit: GR32_Filters
entity: TLUT8
kind: Type
declaration: "TLUT8 = array [Byte] of Byte;"
summary: "256-element byte lookup table array used for 8-bit color channel mapping and tone adjustments."
---

## Description

`TLUT8` defines a 256-element array mapping input byte channel values (`0..255`) to output byte channel values (`0..255`).

It is passed to `ApplyLUT` for brightness, contrast, gamma, and color curve filter operations.

## Example

```pascal
var
  InvertLUT: TLUT8;
  I: Integer;
begin
  // Initialize inverted lookup table
  for I := 0 to 255 do
    InvertLUT[I] := 255 - I;

  ApplyLUT(Bitmap, InvertLUT, True);
end;
```

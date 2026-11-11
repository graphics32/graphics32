---
layout: doc
docType: api
unit: GR32_Paths
parent: TCanvas32
entity: TCanvas32.Bitmap
kind: Property
declaration: "property Bitmap: TBitmap32 read FBitmap;"
summary: "Read-only reference to the target TBitmap32 instance on which vector paths are rendered."
---

## Description

`Bitmap` returns a read-only reference to the target [[TBitmap32]] surface passed to [[TCanvas32.Create]].

`TCanvas32` does not own `Bitmap`; freeing `TCanvas32` does not destroy the underlying bitmap object.

## Example

```pascal
var
  Bitmap: TBitmap32;
  Canvas: TCanvas32;
begin
  Bitmap := TBitmap32.Create(300, 200);
  try
    Canvas := TCanvas32.Create(Bitmap);
    try
      // Verify target bitmap dimensions via Canvas.Bitmap
      Assert(Canvas.Bitmap.Width = 300);
      Assert(Canvas.Bitmap.Height = 200);
    finally
      Canvas.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;
```

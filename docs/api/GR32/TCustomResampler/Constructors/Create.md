---
layout: doc
docType: api
unit: GR32
parent: TCustomResampler
entity: TCustomResampler.Create
kind: Constructor
scope: Public
summary: "Initializes a new instance of TCustomResampler, optionally setting the associated source bitmap."
overloads:
  - signature: "constructor Create; overload; virtual;"
    summary: "Initializes a new instance of TCustomResampler with default PixelAccessMode set to pamSafe."
  - signature: "constructor Create(ABitmap: TCustomBitmap32); overload; virtual;"
    summary: "Initializes a new instance of TCustomResampler and attaches it to a specified source bitmap."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "The source bitmap to associate with this resampler."
---

## Description

`Create` initializes a new `TCustomResampler` instance.

When constructed using `Create(ABitmap)`, the resampler's `Bitmap` property is assigned to `ABitmap`, and the resampler is automatically set as the `Resampler` property of `ABitmap`.

By default, `PixelAccessMode` is initialized to `pamSafe`.

## Example

```pascal
var
  Bitmap: TBitmap32;
  Resampler: TLinearResampler;
begin
  Bitmap := TBitmap32.Create(800, 600);
  try
    // Attach a linear resampler to the bitmap
    Resampler := TLinearResampler.Create(Bitmap);
    // Bitmap now uses Resampler for stretch and sample operations
  finally
    Bitmap.Free;
  end;
end;
```

---
layout: doc
docType: api
unit: GR32.Transpose
entity: Transpose32
kind: Function
summary: "Transposes a 32-bit bitmap or raw pixel buffer by swapping its rows and columns."
overloads:
  - signature: "procedure Transpose32(Src, Dst: TBitmap32); overload;"
    summary: "Transposes a source TBitmap32 into a destination TBitmap32, resizing Dst to (Src.Height, Src.Width)."
    parameters:
      - name: Src
        type: TBitmap32
        description: "Source 32-bit bitmap."
      - name: Dst
        type: TBitmap32
        description: "Destination 32-bit bitmap, automatically resized to match transposed dimensions."

  - signature: "procedure Transpose32(Src, Dst: Pointer; SrcWidth, SrcHeight: Integer); overload;"
    summary: "Transposes a raw 32-bit pixel memory buffer of size SrcWidth x SrcHeight into Dst."
    parameters:
      - name: Src
        type: Pointer
        description: "Pointer to source 32-bit pixel buffer."
      - name: Dst
        type: Pointer
        description: "Pointer to destination 32-bit pixel buffer (must be pre-allocated to hold SrcWidth * SrcHeight pixels)."
      - name: SrcWidth
        type: Integer
        description: "Width of source buffer in pixels."
      - name: SrcHeight
        type: Integer
        description: "Height of source buffer in pixels."
seealso:
  - ReferenceTranspose32
---

## Description

`Transpose32` performs high-performance matrix transposition on 32-bit bitmap data. It automatically uses the fastest available CPU implementation (such as SSE2 SIMD optimization or cache-oblivious recursive memory transposition) registered via Graphics32 function bindings.

When called with `TBitmap32` instances, `Dst` is automatically resized to `(Src.Height, Src.Width)` before copying transposed pixels.

## Example

```pascal
var
  SrcBmp, DstBmp: TBitmap32;
begin
  SrcBmp := TBitmap32.Create(800, 600);
  DstBmp := TBitmap32.Create;
  try
    // Transpose 800x600 bitmap into 600x800 bitmap
    Transpose32(SrcBmp, DstBmp);
    // DstBmp.Width is now 600, DstBmp.Height is now 800
  finally
    SrcBmp.Free;
    DstBmp.Free;
  end;
end;
```

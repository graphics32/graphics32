---
layout: doc
docType: api
unit: GR32
entity: TBitmap32.Create
kind: Constructor
declaration: |
  constructor Create; overload; override;
  constructor Create(AWidth, AHeight: Integer); overload;
  constructor Create(ABackendClass: TCustomBackendClass); overload;
summary: "Instantiates a new instance of TBitmap32 with optional width, height, or custom backend surface class."
parameters:
  - name: AWidth
    type: Integer
    description: "Initial width of the bitmap in pixels."
  - name: AHeight
    type: Integer
    description: "Initial height of the bitmap in pixels."
  - name: ABackendClass
    type: TCustomBackendClass
    description: "Custom memory surface backend class (e.g. GDI, MMF, or Cairo backend)."
---

## Example

```pascal
var
  Bmp: TBitmap32;
begin
  // Create an 800x600 bitmap
  Bmp := TBitmap32.Create(800, 600);
  try
    Bmp.Clear(clWhite32);
  finally
    Bmp.Free;
  end;
end;
```

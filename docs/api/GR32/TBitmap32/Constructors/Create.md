---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Create
kind: Constructor
summary: "Instantiates a new TBitmap32 object."
overloads:
  - signature: "constructor Create; overload; override;"
    summary: "Creates an empty TBitmap32 object with default dimensions (0x0)."
  - signature: "constructor Create(Width, Height: Integer); overload;"
    summary: "Creates a TBitmap32 object initialized with the specified width and height."
    parameters:
      - name: Width
        type: Integer
        description: "Initial width of the bitmap in pixels."
      - name: Height
        type: Integer
        description: "Initial height of the bitmap in pixels."
  - signature: "constructor Create(ABackendClass: TCustomBackendClass); overload;"
    summary: "Creates a TBitmap32 object with a custom backend surface class."
    parameters:
      - name: ABackendClass
        type: TCustomBackendClass
        description: "Custom memory surface backend class (e.g. Memory, GDI, or MMF backend)."
---

## Example

```pascal
var
  Bmp: TBitmap32;
begin
  // Create a 800x600 bitmap and fill it with the color red
  Bmp := TBitmap32.Create(800, 600);
  try
    Bmp.Clear(clRed32);
  finally
    Bmp.Free;
  end;
end;
```

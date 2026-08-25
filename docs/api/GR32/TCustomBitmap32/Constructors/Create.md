---
inheritedFrom: TCustomMap.Create
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Create
kind: Constructor
scope: Public
summary: "Instantiates a new 32-bit bitmap object."
overloads:
  - signature: "constructor Create; overload; override;"
    summary: "Creates an empty bitmap object with default dimensions (0x0)."
  - signature: "constructor Create(AWidth, AHeight: Integer); overload;"
    summary: "Creates a bitmap object initialized with the specified width and height."
    parameters:
      - name: AWidth
        type: Integer
        description: "Initial width of the bitmap in pixels."
      - name: AHeight
        type: Integer
        description: "Initial height of the bitmap in pixels."
  - signature: "constructor Create(ABackendClass: TCustomBackendClass); overload;"
    summary: "Creates a TBitmap32 object with a custom backend surface class."
    parameters:
      - name: ABackendClass
        type: TCustomBackendClass
        description: "Custom memory surface backend class (e.g. Memory, GDI, or MMF backend)."
---

`Create` allocates memory for the pixel buffer and initializes internal backend interfaces.
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

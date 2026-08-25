---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.BeginMeasuring
kind: Method
scope: Public
declaration: "procedure BeginMeasuring(const Callback: TAreaChangedEvent);"
summary: "Begins measuring modified pixel regions and accumulates area change notifications."
parameters:
  - name: Callback
    type: TAreaChangedEvent
    description: "Callback procedure invoked when measuring ends with the accumulated changed region rectangle."
---

## Description

`BeginMeasuring` puts the bitmap into measuring mode. Subsequent drawing operations will not immediately fire area change events; instead, the modified rectangle areas are tracked and combined into a single bounding rectangle until `EndMeasuring` is called.

## Example

```pascal
Bitmap.BeginMeasuring(OnAreaMeasured);
try
  Bitmap.Line(0, 0, 100, 100, clRed32);
  Bitmap.FillRect(20, 20, 80, 80, clBlue32);
finally
  Bitmap.EndMeasuring;
end;
```

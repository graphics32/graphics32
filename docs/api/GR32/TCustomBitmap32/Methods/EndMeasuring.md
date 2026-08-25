---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.EndMeasuring
kind: Method
scope: Public
declaration: "procedure EndMeasuring;"
summary: "Concludes area change measurement and invokes the registered measuring callback procedure."
---

## Description

`EndMeasuring` exits measuring mode and invokes the callback provided in `BeginMeasuring` with the bounding rectangle of all changed areas.

## Example

```pascal
Bitmap.BeginMeasuring(OnAreaMeasured);
try
  // Perform drawing operations
finally
  Bitmap.EndMeasuring;
end;
```

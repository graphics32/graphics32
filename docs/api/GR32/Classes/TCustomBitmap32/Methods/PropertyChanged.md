---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.PropertyChanged
kind: Method
scope: Public
declaration: "procedure PropertyChanged; virtual;"
summary: "Notifies the bitmap and active backend instance that a property setting has been modified."
---

## Description

`PropertyChanged` is called internally when properties such as `DrawMode`, `CombineMode`, or `WrapMode` are modified to update backend state and trigger change notifications.

## Example

```pascal
Bitmap.PropertyChanged;
```

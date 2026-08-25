---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Changed
kind: Method
scope: Public
summary: "Triggers change notifications for the entire bitmap or a specified sub-rectangle region."
overloads:
  - signature: "procedure Changed; overload; override;"
    summary: "Triggers change notification across the entire bitmap surface."
  - signature: "procedure Changed(const Area: TRect; const Info: Cardinal = AREAINFO_RECT); reintroduce; overload; virtual;"
    summary: "Triggers change notification for a specified modified sub-rectangle region."
    parameters:
      - name: Area
        type: TRect
        description: "Modified sub-rectangle area of the bitmap surface."
      - name: Info
        type: Cardinal
        description: "Area information flag (default AREAINFO_RECT)."
---

## Description

`Changed` signals that pixel contents have been modified.

Calling `Changed` invalidates backend cache surfaces and fires `OnChange` and `OnAreaChanged` events.

## Example

```pascal
Bitmap.Changed(Rect(10, 10, 50, 50));
```

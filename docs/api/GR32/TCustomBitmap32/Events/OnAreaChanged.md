---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.OnAreaChanged
kind: Event
scope: Published
declaration: "property OnAreaChanged: TAreaChangedEvent read FOnAreaChanged write FOnAreaChanged;"
summary: "Fired when a specific sub-rectangle region of the pixel buffer is modified."
---

## Description

`OnAreaChanged` passes the modified `TRect` and area change info flags to subscribers when pixel data changes.

## Signature

```pascal
type TAreaChangedEvent = procedure(Sender: TObject; const Area: TRect; const Info: Cardinal) of object;
```

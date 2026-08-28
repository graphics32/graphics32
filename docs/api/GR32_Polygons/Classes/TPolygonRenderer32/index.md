---
layout: doc
docType: api
unit: GR32_Polygons
entity: TPolygonRenderer32
kind: Class
aliases: [TPolygonRenderer32Class]
declaration: "TPolygonRenderer32 = class abstract(TCustomPolygonRenderer)"
inheritance:
  - TCustomPolygonRenderer
  - TThreadPersistent
  - TNotifiablePersistent
  - TPlainInterfacedPersistent
  - TPersistent
  - TObject
summary: "Abstract base class for 32-bit color vector polygon renderers targetted at TCustomBitmap32."
---

## Description

`TPolygonRenderer32` is the base class for polygon renderers that draw onto a 32-bit destination bitmap ([[TCustomBitmap32]]). It manages properties such as destination bitmap reference (`Bitmap`), fill mode (`FillMode`), solid fill color (`Color`), and optional custom span filler (`Filler`).

---

[members]

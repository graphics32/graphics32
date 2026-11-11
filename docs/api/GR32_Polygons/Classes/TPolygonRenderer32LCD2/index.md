---
layout: doc
docType: api
unit: GR32_Polygons
entity: TPolygonRenderer32LCD2
kind: Class
declaration: "TPolygonRenderer32LCD2 = class(TPolygonRenderer32LCD)"
inheritance:
  - TPolygonRenderer32LCD
  - TPolygonRenderer32VPR
  - TPolygonRenderer32
  - TCustomPolygonRenderer
  - TThreadPersistent
  - TNotifiablePersistent
  - TPlainInterfacedPersistent
  - TPersistent
  - TObject
summary: "Polygon renderer providing soft sub-pixel LCD antialiasing transitions."
---

## Description

`TPolygonRenderer32LCD2` extends `TPolygonRenderer32LCD` by applying a 3-tap FIR low-pass filter across adjacent sub-pixel coverage values, producing softer color transitions and minimizing color fringing on LCD screens.

[members]
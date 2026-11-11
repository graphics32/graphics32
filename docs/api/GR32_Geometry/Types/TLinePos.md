---
layout: doc
docType: api
unit: GR32_Geometry
entity: TLinePos
kind: Type
summary: "Specifies which endpoint(s) of a polyline or line segment should be shortened."
---

## Description

`TLinePos` is an enumeration type used by the `Shorten` routine to designate which endpoint(s) of a line segment or polyline array should be adjusted or trimmed by a specified delta distance.

## Values

| Value | Description |
| --- | --- |
| `lpStart` | Shortens only the starting point of the line or polyline. |
| `lpEnd` | Shortens only the ending point of the line or polyline. |
| `lpBoth` | Shortens both the starting and ending points of the line or polyline. |
| `lpNeither` | Leaves both endpoints unchanged. |

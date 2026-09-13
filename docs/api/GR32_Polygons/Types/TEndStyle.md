---
layout: doc
docType: api
unit: GR32_Polygons
entity: TEndStyle
aliases: [TEndStyles]
kind: Type
declaration: |
  TEndStyle = (esButt, esSquare, esRound);
  TEndStyles = set of TEndStyle;
summary: "Enumeration specifying line end cap styles for open polyline strokes."
---

## Description

`TEndStyle` defines how the open start and end endpoints of polyline paths are capped during stroke outline generation in routines such as [[PolylineFS]] and [[PolyPolylineFS]].

### Enum Values

| Value | Description | Example |
| --- | --- | --- |
| `esButt` | Endpoints are cut off flat exactly at the endpoint coordinates. | ![](/images/JoinStyle-EndStyle-jsRound-esButt.png) |
| `esSquare` | Endpoints are extended past the endpoint by half the line width with flat square caps. | ![](/images/JoinStyle-EndStyle-jsRound-esSquare.png) |
| `esRound` | Endpoints are capped with semicircular rounded ends. | ![](/images/JoinStyle-EndStyle-jsRound-esRound.png) |

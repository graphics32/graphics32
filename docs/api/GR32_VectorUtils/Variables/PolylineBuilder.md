---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: PolylineBuilder
kind: Variable
declaration: "var PolylineBuilder: TPolylineBuilderClass;"
summary: "Global active polyline builder class reference used for stroke outline building and polygon offsetting."
---

## Description

`PolylineBuilder` points to the active `TPolylineBuilderClass` backend implementation selected at compile time or initialization. All polyline stroking routines (`BuildPolyLine`, `BuildPolyPolyLine`) and polygon inflation routines (`Grow`) delegate outline construction to this class reference.

### Controlling Compiler Defines

The active `PolylineBuilder` class assigned during unit initialization is determined by controlling conditional compiler defines:

| Compiler Define | PolylineBuilder Class | Description |
|---|---|---|
| `GR32_OFFSET_REF` | `PolyLineBuilderReference` | Pure Pascal reference polyline builder implementation provided by [[GR32_VectorUtils.Reference]]. |
| `GR32_OFFSET_ANGUS` | `PolyLineBuilderAngus` | A custom offsetter by Angus Johnson provided by [[GR32_VectorUtils.Angus]]. |
| `GR32_OFFSET_CLIPPER` | `PolyLineBuilderClipper` | Clipper2 offsetting engine implementation provided by [[GR32_VectorUtils.Clipper2]]. |

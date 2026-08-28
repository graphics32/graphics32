---
layout: doc
docType: api
unit: GR32_Polygons
entity: IPolygonRendererBatching
kind: Interface
declaration: "IPolygonRendererBatching = interface"
summary: "Interface for polygon renderers supporting batched drawing operations."
---

## Description

`IPolygonRendererBatching` is an optional interface implemented by custom polygon renderers that support batching multiple polygon draw calls together (e.g. for GPU-accelerated or cached vector rendering).

## Methods

### BeginDraw
```pascal
procedure BeginDraw;
```
Prepares the renderer for a batch of drawing operations.

### EndDraw
```pascal
procedure EndDraw;
```
Flushes and finalizes the batched drawing operations.

---
layout: doc
docType: api
unit: GR32_Polygons
parent: TCustomPolygonFiller
entity: TCustomPolygonFiller.EndRendering
kind: Method
declaration: "procedure EndRendering; virtual;"
summary: "Finalizes resources after scanline rendering completes."
---

## Description

`EndRendering` is called by polygon renderers after all scanline spans have been rendered. Subclasses override this method to clean up temporary resources.

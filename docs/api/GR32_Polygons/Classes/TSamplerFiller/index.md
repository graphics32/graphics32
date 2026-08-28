---
layout: doc
docType: api
unit: GR32_Polygons
entity: TSamplerFiller
kind: Class
declaration: "TSamplerFiller = class(TCustomPolygonFiller)"
inheritance:
  - TCustomPolygonFiller
  - TObject
summary: "Polygon filler that uses a 2D custom sampler (TCustomSampler) to sample span pixels."
---

## Description

`TSamplerFiller` bridges Graphics32 sampling algorithms ([[TCustomSampler]]) with polygon span filling. It calls `Sampler.GetSampleInt` for each pixel in a scanline span to generate custom color gradients, procedural textures, or resampled bitmap colors.

---

[members]

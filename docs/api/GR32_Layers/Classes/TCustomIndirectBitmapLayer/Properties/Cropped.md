---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomIndirectBitmapLayer
entity: TCustomIndirectBitmapLayer.Cropped
kind: Property
scope: Public
declaration: "property Cropped: Boolean read FCropped write SetCropped;"
summary: "Controls whether layer rendering is cropped to the parent control's image bounds."
---

## Description

When `Cropped` is `True`, bitmap rendering is clipped to the bitmap rectangle of the owner `TCustomImage32`.

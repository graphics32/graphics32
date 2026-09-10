---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.ForceFullInvalidate
kind: Method
scope: Public
declaration: "procedure ForceFullInvalidate; virtual;"
summary: "Forces entire buffer area to be marked invalid for full repainting."
---

## Description

`ForceFullInvalidate` discards any pending partial invalid region optimization and schedule a full repaint of the entire control buffer.

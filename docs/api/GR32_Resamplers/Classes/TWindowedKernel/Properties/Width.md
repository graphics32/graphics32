---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TWindowedKernel
entity: TWindowedKernel.Width
kind: Property
scope: Published
declaration: "property Width: TFloat read FWidth write SetWidth;"
summary: "Specifies the effective spatial window radius (half-width) in pixels."
---

## Description

`Width` sets the spatial cutoff window radius $W$ in pixels. Values outside $[-W, W]$ are truncated to zero weight by the kernel filter.

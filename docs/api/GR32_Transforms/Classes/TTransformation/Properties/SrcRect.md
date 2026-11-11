---
layout: doc
docType: api
unit: GR32_Transforms
parent: TTransformation
entity: TTransformation.SrcRect
kind: Property
declaration: "property SrcRect: TFloatRect read FSrcRect write SetSrcRect;"
summary: "Defines the source rectangle bounds for transformation calculations."
---

## Description

`SrcRect` defines the source coordinate bounds $[Left, Top, Right, Bottom]$ used by transformations to normalize local coordinates.

::: info Note
Since `SrcRect` is a [[TFloatRect]], the boundary has floating point coordinates.
:::
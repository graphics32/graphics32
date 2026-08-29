---
layout: doc
docType: api
unit: GR32_Transforms
parent: TProjectiveTransformationEx
entity: TProjectiveTransformationEx.Extrapolate
kind: Property
declaration: "property Extrapolate: boolean read FExtrapolate write FExtrapolate;"
summary: "Controls whether pixels beyond destination quadrilateral bounds are transformed."
---

## Description

When `Extrapolate` is set to `True`, `GetTransformedBounds` returns full source rectangle bounds, enabling resampling for pixels outside destination quadrilateral boundaries.

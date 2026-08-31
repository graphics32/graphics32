---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomGradientSampler
entity: TCustomGradientSampler.WrapMode
kind: Property
declaration: "property WrapMode: TWrapMode read FWrapMode write SetWrapMode;"
summary: "Specifies coordinate wrap mode behavior when sampling outside the normalized [0, 1] domain."
---

## Description

`WrapMode` controls coordinate domain repetition:
- `wmClamp`: Clamps values $< 0$ to 0.0 and $> 1$ to 1.0.
- `wmRepeat`: Wraps values periodically via fractional part ($u - \lfloor u floor$).
- `wmMirror`: Mirrors values symmetrically across integer boundaries.
- `wmReflect`: Reflects values back and forth between 0 and 1.

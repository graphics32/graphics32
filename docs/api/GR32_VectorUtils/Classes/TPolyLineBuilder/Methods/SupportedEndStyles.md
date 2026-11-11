---
layout: doc
docType: api
unit: GR32_VectorUtils
parent: TPolyLineBuilder
entity: TPolyLineBuilder.SupportedEndStyles
kind: Method
declaration: "class function SupportedEndStyles: TEndStyles; virtual;"
summary: "Returns the set of line end cap styles (TEndStyles) supported by this builder backend."
returns:
  - type: TEndStyles
    description: "A set of [[TEndStyles]] enum flags representing supported line end cap styles."
---

## Description

`SupportedEndStyles` returns the set of end cap styles (`esButt`, `esSquare`, `esRound`) supported by the concrete `TPolyLineBuilder` backend class.

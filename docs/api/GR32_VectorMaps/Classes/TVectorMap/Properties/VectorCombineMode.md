---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.VectorCombineMode
kind: Property
declaration: "property VectorCombineMode: TVectorCombineMode read FVectorCombineMode write SetVectorCombineMode;"
summary: "Specifies vector combination strategy during vector map merge operations."
---

## Description

`VectorCombineMode` controls how displacement vectors are blended or replaced during [[TVectorMap.Merge]] operations (`vcmAdd`, `vcmReplace`, or `vcmCustom`).

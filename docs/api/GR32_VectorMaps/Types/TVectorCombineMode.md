---
layout: doc
docType: api
unit: GR32_VectorMaps
entity: TVectorCombineMode
kind: Type
declaration: "TVectorCombineMode = (vcmAdd, vcmReplace, vcmCustom);"
summary: "Specifies how displacement vectors are combined during vector map merge operations."
---

## Description

`TVectorCombineMode` determines the vector blending or assignment strategy when combining or merging displacement vectors in a [[TVectorMap]].

## Values

| Value | Description |
| --- | --- |
| `vcmAdd` | Adds source displacement vectors to existing destination displacement vectors. |
| `vcmReplace` | Replaces destination displacement vectors with source displacement vectors. |
| `vcmCustom` | Invokes the [[TVectorMap.OnVectorCombine]] event handler to calculate combined vectors. |

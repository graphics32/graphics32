---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.Merge
kind: Method
declaration: "procedure Merge(DstLeft, DstTop: Integer; Src: TVectorMap; SrcRect: TRect);"
summary: "Merges a source vector map region into this vector map using current VectorCombineMode."
parameters:
  - name: DstLeft, DstTop
    type: Integer
    description: "Destination top-left corner coordinates."
  - name: Src
    type: TVectorMap
    description: "Source vector map to copy or blend vectors from."
  - name: SrcRect
    type: TRect
    description: "Source sub-rectangle."
---

## Description

`Merge` overlays displacement vectors from `Src` rectangle `SrcRect` onto this vector map at `(DstLeft, DstTop)`. Vector combination behavior is governed by [[TVectorMap.VectorCombineMode]] (`vcmAdd`, `vcmReplace`, or `vcmCustom`).

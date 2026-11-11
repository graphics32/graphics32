---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.LoadFromFile
kind: Method
declaration: "procedure LoadFromFile(const FileName: string);"
summary: "Loads displacement vectors from an Adobe Photoshop Liquify mesh file."
parameters:
  - name: FileName
    type: string
    description: "Path to the Photoshop Liquify mesh file."
---

## Description

`LoadFromFile` imports displacement vector grid data from an Adobe Photoshop Liquify mesh file (`.msh`).

::: info Note
The method resizes the vector map buffer to match mesh grid dimensions (`Width`, `Height`).
:::
